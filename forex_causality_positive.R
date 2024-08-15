library(dplyr)
library(readxl)
library(xts)
library(DBI)
library(fUnitRoots)
library(urca)
library(vars)
library(aod)
library(zoo)
library(tseries)
library(bootUR)
library(readr)
library(Rbeast)
library(purrr)


# Prevent scientific notation
options(scipen=999)
# Decimals to be saved
n_dec <- 7

# Connect to DB
data_path <- file.path(getwd(), "elaborations")

cpi_data <- "cpi_positive"
target <- "forex_causality_positive"

con_cpi <- dbConnect(RSQLite::SQLite(),
                     dbname = file.path(data_path, paste0(cpi_data, ".db")))

con_target <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(target, ".db")))

# Get structural break probability for CPI
cpi_df <- data.frame(date=as.Date("2021-02-21"))
for (table in dbListTables(con_cpi)) {
    df <- dbReadTable(con_cpi, table)
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    df <- df %>% dplyr::select(date, cp_prob)
    colnames(df) <- c("date", table)
    cpi_df <- full_join(cpi_df, df, by= "date")
}

cpi_ts <-as.xts(subset(cpi_df, select=-c(date)), order.by = cpi_df$date)

cpi_ts$average <- rowMeans(cpi_ts)

# Get forex data
forex_df <- read_csv("official_data/RUBUSD.csv",
                     col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                      Open = col_skip(), High = col_skip(),
                                      Low = col_skip(), `Adj Close` = col_skip(),
                                      Volume = col_skip()))


# Manipulate Forex data ans get structural break probability via BEAST
forex_grouped_df <- forex_df %>%
    filter(Date >= as.Date("2021-02-14")) %>%
    filter(Date < as.Date("2022-10-10")) %>% # Removing data after CPI cutoff...to be updated over time
    group_by(week = cut(Date, "week")) %>%
    mutate(date = as.Date(week) + 6)  %>%  # Move reference to the end of the week
    mutate(forex=mean(Close)) %>%
    ungroup() %>%
    distinct(date, forex)

forex_ts <- as.xts(forex_grouped_df$forex, order.by = forex_grouped_df$date)
names(forex_ts) <- "forex"
# Compute Beast algorithm
res_beast <- beast(forex_ts, season = "none")
forex_ts$cp_prob <- round(res_beast[["trend"]][["pos_cpOccPr"]], n_dec)

##### FOREX CAUSALITY #####
for (tab in names(cpi_ts)){
    tryCatch({
        ##### CPI TY Causality #####
        # Check order of integration for the time series
        cpi_order_integration <- order_integration(
            merge(forex_ts$cp_prob, cpi_ts[,tab]),
            max_order = 5)
        # Select max order of integration
        var_cpi_select <- VARselect(merge(forex_ts$cp_prob, cpi_ts[,tab]),
                                    lag.max = 12,
                                    type = "both")
        # Selecting the lag for VAR
        AIC_cpi <- var_cpi_select$selection[[1]]
        cpi_max_ord <- max(cpi_order_integration$order_int)
        total_cpi_lag <- AIC_cpi + cpi_max_ord

        # Performing VAR
        var_cpi <- VAR(
            merge(forex_ts$cp_prob, cpi_ts[,tab]),
            p=total_cpi_lag,
            type="both")


        cpi_serial <- serial.test(var_cpi, type="BG", lags.pt = 52, lags.bg=52)
        if(1/roots(var_cpi)[[1]] > 1 || 1/roots(var_cpi)[[2]] > 1){
            roots_cpi <- "stable"
        } else {
            roots_cpi <- "not stable"
        }


        # Causality from first column (forex) to second (breaks)
        wt2_cpi<-wald.test(
            b=coef(var_cpi$varresult[[2]]),
            Sigma=vcov(var_cpi$varresult[[2]]),
            Terms=c(seq(1, AIC_cpi*2, 2)))
        cpi_causality_p <- wt2_cpi[["result"]][["chi2"]][["P"]]

        # Saving results
        results_df <- data.frame(
            item = c("cpi"),
            aic_lag = c(AIC_cpi),
            max_int_order = c(cpi_max_ord),
            serial_ac_p = c(cpi_serial$serial$p.value),
            root = c(roots_cpi),
            causality_p = c(cpi_causality_p)
        )
        dbWriteTable(con_target, tab, results_df, overwrite = TRUE)
    }, error = function(e) {
        cat("Error for category ", tab, ": ", conditionMessage(e), "\n")
        #next
    })
}



# Disconnect from DB
dbDisconnect(conn = con_cpi)
dbDisconnect(conn = con_target)

