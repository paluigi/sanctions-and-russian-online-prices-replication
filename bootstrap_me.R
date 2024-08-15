library(dplyr)
library(DBI)
library(imputeTS)
library(meboot)
library(boot)
library(urca)

# Prevent scientific notation
options(scipen=999)
# Decimals to be saved
n_dec <- 4

# War start
war_start <- as.Date("2022-02-24", format = "%Y-%m-%d")

# Connect to DB
data_path <- file.path(getwd(), "elaborations")
origin <- "cpi_imputed"
target <- "meboot_tests"

con_origin <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(origin, ".db")))

con_target <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(target, ".db")))


# List to save results for each table
results_df <- data.frame()


##### LOOP ####

for (table in dbListTables(con_origin)) {
    df <- dbReadTable(con_origin, table)
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    df <- df %>%
        filter(!is.na(official)) %>%
        mutate(index_complete = na_kalman(index)) %>%
        mutate(diff = index_complete - official) %>%
        select(c(official, index_complete, date))

    set.seed(12345)
    official_boot <- meboot(df$official, reps=99)
    ws_boot <- meboot(df$index_complete, reps=99)

    official_panel <- cbind(official_boot$x, official_boot$ensemble)
    ws_panel <- cbind(ws_boot$x, ws_boot$ensemble)

    # List to save results for each table
    results_inner_df <- data.frame()
    # Seed fore reproducibility
    set.seed(12345)
    for (i in (1:ncol(official_panel))) {
        for (j in (1:ncol(ws_panel))) {
            diff <- official_panel[,i] - ws_panel[,j]
            adf_test_diff <- ur.df(diff, type="none", lags=6, selectlags = "AIC")
            # Results in one object
            results_inner <- data.frame(
                adf_stat = adf_test_diff@teststat,
                adf_zcoeff = adf_test_diff@testreg$coefficients[1,1],
                adf_zse = adf_test_diff@testreg$coefficients[1,2]
            )
            colnames(results_inner) <- c("adf_stat", "adf_zcoeff", "adf_zse")

            results_inner_df <- bind_rows(results_inner_df, results_inner)
        }
    }

    # Calculate statistics on original series, overwrite bootstrapped variables
    diff <- df$official - df$index_complete
    adf_test_diff <- ur.df(diff, type="none", lags=6, selectlags = "AIC")

    simple_quant <- quantile(results_inner_df$adf_zcoeff, c(0.025, 0.975), type = 8)
    out <- list(
        t=as.matrix(results_inner_df$adf_zcoeff),
        t0=adf_test_diff@testreg$coefficients[1,1],
        var.t0=(adf_test_diff@testreg$coefficients[1,2])^2,
        R=10000
    )
    class(out) <- "boot"
    boot_quant <- boot.ci(out, type = "perc")$percent[4:5]
    res <- as.data.frame(rbind(simple_quant, boot_quant))
    res$method <- rownames(res)
    res$table <- table

    results_df <- bind_rows(results_df, res)
    # Revert date to character for saving
    dbWriteTable(con_target, table, res, overwrite = TRUE)
}

dbWriteTable(con_target, "summary", results_df, overwrite = TRUE)

# Disconnect from DB
dbDisconnect(conn = con_origin)
dbDisconnect(conn = con_target)


