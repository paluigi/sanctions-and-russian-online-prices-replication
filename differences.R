library(dplyr)
library(xts)
library(Rbeast)
library(DBI)
library(ie2misc)
library(LongMemoryTS)
library(imputeTS)
library(urca)

# Prevent scientific notation
options(scipen=999)
# Decimals to be saved
n_dec <- 4

# War start
war_start <- as.Date("2022-02-24", format = "%Y-%m-%d")

# Parameters for cointegration testing
m1<-floor(1+20^0.45)
m<-floor(1+20^0.35)

# Connect to DB
data_path <- file.path(getwd(), "elaborations")

origin <- "cpi"
target <- "ts_analysis"


con_origin <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(data_path, paste0(origin, ".db")))

con_target <- dbConnect(RSQLite::SQLite(),
                      dbname = file.path(data_path, paste0(target, ".db")))

# List to save results for each table
results_df <- data.frame()


# For each table:
# - Calculate differences between index and official, APE and ALPE
# - Calculate BEAST profile for differences, APE and ALPE
# - Save back the results to target DB
# Calculate MAPE, MALPE (overall, pre, post), StDev, and T-Test
# Calculate cointegration for official and index_complete (only overall, different methodologies)
# Calculate stationariety of differences (only overall)

for (table in dbListTables(con_origin)) {
    df <- dbReadTable(con_origin, table)
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    
    df <- df %>%
        filter(!is.na(official)) %>%
        mutate(index_complete = na_kalman(index)) %>%
        mutate(diff = index_complete - official) %>%
        mutate(ape = abs(diff/official)*100) %>%
        mutate(alpe = (diff/official)*100)

    for (s in c("diff", "ape", "alpe")) {
        ts <- as.xts(df[[s]], order.by = df$date)
        res_beast <- beast(ts, season = "none")
        ts <- data.frame(date=index(ts), coredata(ts))
        ts$cp_prob <- round(res_beast[["trend"]][["cpOccPr"]], n_dec)
        ts$slope_pos <- round(res_beast[["trend"]][["slpSgnPosPr"]], n_dec)
        ts$slope_zero <- round(res_beast[["trend"]][["slpSgnZeroPr"]], n_dec)
        colnames(ts) <- c("date", s, paste0(s,"_cp_prob"), paste0(s,"_slope_pos"),
                          paste0(s,"_slope_zero"))
        ts <- dplyr::select(ts, -c(s))
        df <- merge(df, ts, by="date")
    }

    df_pre <- df %>% filter(date < war_start)
    df_post <- df %>% filter(date > war_start)

    # Cointegration testing
    coint_matrix <- df %>% dplyr::select(index_complete, official) %>% as.matrix()
    # RY2002 test for equality of integration rank (null: same rank)
    d.hat<-apply(coint_matrix,2,lW_wrap, m=m1)
    same_integration_test <- T.rho(data=coint_matrix, d.hat=d.hat, m=m, m1=m1)
    # MV2004 test for cointegration
    mv2004_test <- FCI_MV04(coint_matrix, type="const", N=m, m=m)
    # NS2007 test for cointegration rank
    ns2007_rank <- FCI_NS07(coint_matrix, m=m, m1=m1, mean_correct = "mean")
    # ZRY2018 test for cointegration rank
    zry2018_rank <- FCI_ZRY18(coint_matrix, lag_max=5, lag_max2 = 15, c0 = 0.3)

    # Unit roots test for differences
    adf_test_diff <- ur.df(df$diff, type="none", lags=6, selectlags = "AIC")
    kpss_test_diff <- ur.kpss(df$diff, type = "mu", lags="long")

    # Unit roots test for alpe
    adf_test_alpe <- ur.df(df$alpe, type="none", lags=6, selectlags = "AIC")
    kpss_test_alpe <- ur.kpss(df$alpe, type = "mu", lags="long")


    # Results in one object
    results <- data.frame(
        table = table,
        mape = round(mean(df$ape), n_dec),
        sd_ape = round(sd(df$ape), n_dec),
        malpe = round(mean(df$alpe), n_dec),
        sd_alpe = round(sd(df$alpe), n_dec),
        mape_pre = round(mean(df_pre$ape), n_dec),
        sd_ape_pre = round(sd(df_pre$ape), n_dec),
        malpe_pre = round(mean(df_pre$alpe), n_dec),
        sd_alpe_pre = round(sd(df_pre$alpe), n_dec),
        mape_post = round(mean(df_post$ape), n_dec),
        sd_ape_post = round(sd(df_post$ape), n_dec),
        malpe_post = round(mean(df_post$alpe), n_dec),
        sd_alpe_post = round(sd(df_post$alpe), n_dec),
        mape_diff_pval = round(t.test(df_pre$ape, df_post$ape)$p.value, n_dec),
        malpe_diff_pval = round(t.test(df_pre$alpe, df_post$alpe)$p.value, n_dec),
        # Nash-Sutcliffe Modeling efficiecny
        nsme <- round(vnse(df$index_complete, df$official), n_dec),
        ry2002 <- if(same_integration_test$p.val < 0.05) "Reject" else "Not Reject",
        mv2004 <- if(mv2004_test$reject) "Reject" else "Not Reject",
        ns2007 <- ns2007_rank,
        zry2019 <- zry2018_rank,
        adf_diff <- (if(abs(adf_test_diff@teststat) > abs(adf_test_diff@cval[2])) "Reject" else "Not Reject"),
        kpss_diff <- (if(abs(kpss_test_diff@teststat) > abs(kpss_test_diff@cval[2])) "Reject" else "Not reject"),
        adf_alpe <- (if(abs(adf_test_alpe@teststat) > abs(adf_test_alpe@cval[2])) "Reject" else "Not Reject"),
        kpss_alpe <- (if(abs(kpss_test_alpe@teststat) > abs(kpss_test_alpe@cval[2])) "Reject" else "Not reject")
    )

    colnames(results) <- c("table", "mape", "sd_ape", "malpe", "sd_alpe", "mape_pre",
                           "sd_ape_pre", "malpe_pre", "sd_alpe_pre", "mape_post",
                           "sd_ape_post", "malpe_post", "sd_alpe_post", "mape_diff_pval",
                           "malpe_diff_pval", "nsme", "ry2002", "mv2004", "ns2007",
                           "zry2019", "adf_diff", "kpss_diff", "adf_alpe", "kpss_alpe")

    results_df <- bind_rows(results_df, results)
    # Revert date to character for saving
    df$date <- as.character.Date(df$date)
    dbWriteTable(con_target, table, df, overwrite = TRUE)
}

dbWriteTable(con_target, "summary", results_df, overwrite = TRUE)

# Disconnect from DB
dbDisconnect(conn = con_origin)
dbDisconnect(conn = con_target)
