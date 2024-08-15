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

# Prevent scientific notation
options(scipen=999)

# Connect to DB
data_path <- file.path(getwd(), "elaborations")

cpi_data <- "cpi"
stock_data <- "stock"
target <- "ts_analysis_202305_all"
target_fin <- "ts_analysis_202305_fin"
target_trade <- "ts_analysis_202305_trade"


con_cpi <- dbConnect(RSQLite::SQLite(),
                     dbname = file.path(data_path, paste0(cpi_data, ".db")))
con_stock <- dbConnect(RSQLite::SQLite(),
                       dbname = file.path(data_path, paste0(stock_data, ".db")))

con_target <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(target, ".db")))

con_target_fin <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(target_fin, ".db")))

con_target_trade <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(target_trade, ".db")))

# Get sanctions data
sanctions_df <- read_excel("official_data/russia-sanction-timeline.xlsx",
                        col_types = c("date", "text", "text"))


sanctions_all_df <- sanctions_df %>%
    group_by(week = cut(Date, "week")) %>%
    mutate(date = as.Date(week) + 6)  %>%  # Move reference to the end of the week
    mutate(total=n()) %>%
    ungroup() %>%
    distinct(date, total) %>%
    filter(date < as.Date("2022-10-10"))  # Removing data after CPI cutoff...to be updated over time

sanctions_fin_df <- sanctions_df %>%
    filter(`Sanction type` == "financial") %>%
    group_by(week = cut(Date, "week")) %>%
    mutate(date = as.Date(week) + 6)  %>%  # Move reference to the end of the week
    mutate(total=n()) %>%
    ungroup() %>%
    distinct(date, total) %>%
    filter(date < as.Date("2022-10-10"))  # Removing data after CPI cutoff...to be updated over time

sanctions_trade_df <- sanctions_df %>%
    filter(!`Sanction type` == "financial") %>%
    group_by(week = cut(Date, "week")) %>%
    mutate(date = as.Date(week) + 6)  %>%  # Move reference to the end of the week
    mutate(total=n()) %>%
    ungroup() %>%
    distinct(date, total) %>%
    filter(date < as.Date("2022-10-10"))  # Removing data after CPI cutoff...to be updated over time


sanctions_all_ts <- as.xts(subset(sanctions_all_df, select=-c(date)), order.by = sanctions_all_df$date)
# add missing dates
sanctions_all_ts <- merge(sanctions_all_ts, seq.Date(as.Date("2021-02-21"), as.Date("2022-10-09"), by = "week"))
sanctions_all_ts <- na.fill(sanctions_all_ts, 0)

sanctions_fin_ts <- as.xts(subset(sanctions_fin_df, select=-c(date)), order.by = sanctions_fin_df$date)
# add missing dates
sanctions_fin_ts <- merge(sanctions_fin_ts, seq.Date(as.Date("2021-02-21"), as.Date("2022-10-09"), by = "week"))
sanctions_fin_ts <- na.fill(sanctions_fin_ts, 0)

sanctions_trade_ts <- as.xts(subset(sanctions_trade_df, select=-c(date)), order.by = sanctions_trade_df$date)
# add missing dates
sanctions_trade_ts <- merge(sanctions_trade_ts, seq.Date(as.Date("2021-02-21"), as.Date("2022-10-09"), by = "week"))
sanctions_trade_ts <- na.fill(sanctions_trade_ts, 0)


# Get structural break probability for CPI
cpi_df <- data.frame(date=as.Date("2021-02-21"))
for (table in dbListTables(con_cpi)) {
    df <- dbReadTable(con_cpi, table)
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    df <- df %>% dplyr::select(date, cp_prob)
    colnames(df) <- c("date", table)
    cpi_df <- full_join(cpi_df, df, by= "date")
}
# Get structural break probability for Stock
stock_df <- data.frame(date=as.Date("2021-02-21"))
for (table in dbListTables(con_stock)) {
    df <- dbReadTable(con_stock, table)
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    df <- df %>% dplyr::select(date, cp_prob)
    colnames(df) <- c("date", table)
    stock_df <- full_join(stock_df, df, by= "date")
}

cpi_ts <-as.xts(subset(cpi_df, select=-c(date)), order.by = cpi_df$date)
stock_ts <-as.xts(subset(stock_df, select=-c(date)), order.by = stock_df$date)

cpi_ts$average <- rowMeans(cpi_ts)
stock_ts$average <- rowMeans(stock_ts)

##### ALL SANCTIONS #####
for (tab in names(cpi_ts)){
    tryCatch({
        ##### CPI TY Causality #####
        # Check order of integration for the time series
        cpi_order_integration <- order_integration(
            merge(sanctions_all_ts$total, cpi_ts[,tab]),
            max_order = 5)
        # Select max order of integration
        var_cpi_select <- VARselect(merge(sanctions_all_ts$total, cpi_ts[,tab]),
                                    lag.max = 12,
                                    type = "both")
        # Selecting the lag for VAR
        AIC_cpi <- var_cpi_select$selection[[1]]
        cpi_max_ord <- max(cpi_order_integration$order_int)
        total_cpi_lag <- AIC_cpi + cpi_max_ord

        # Performing VAR
        var_cpi <- VAR(
            merge(sanctions_all_ts$total, cpi_ts[,tab]),
            p=total_cpi_lag,
            type="both")


        cpi_serial <- serial.test(var_cpi, type="BG", lags.pt = 52, lags.bg=52)
        if(1/roots(var_cpi)[[1]] > 1 || 1/roots(var_cpi)[[2]] > 1){
            roots_cpi <- "stable"
        } else {
            roots_cpi <- "not stable"
        }

        # Causality from second column (breaks) to first (sanctions)
        wt1_cpi<-wald.test(
            b=coef(var_cpi$varresult[[1]]),
            Sigma=vcov(var_cpi$varresult[[1]]),
            Terms=c(seq(2, AIC_cpi*2, 2)))
        # Not really of interest
        # wt1_cpi[["result"]][["chi2"]][["P"]]

        # Causality from first column (sanctions) to second (breaks)
        wt2_cpi<-wald.test(
            b=coef(var_cpi$varresult[[2]]),
            Sigma=vcov(var_cpi$varresult[[2]]),
            Terms=c(seq(1, AIC_cpi*2, 2)))
        cpi_causality_p <- wt2_cpi[["result"]][["chi2"]][["P"]]
        ##### Stock TY Causality #####
        stock_order_integration <- order_integration(
            merge(sanctions_all_ts$total, stock_ts[,tab]),
            max_order = 5)

        var_stock_select <- VARselect(
            merge(sanctions_all_ts$total, stock_ts[,tab]),
            lag.max = 12,
            type = "both")

        AIC_stock <- var_stock_select$selection[[1]]
        stock_max_ord <- max(stock_order_integration$order_int)
        total_stock_lag <- AIC_stock + cpi_max_ord

        var_stock <- VAR(merge(sanctions_all_ts$total, stock_ts[,tab]),
                         p=total_stock_lag,
                         type="both")


        stock_serial <- serial.test(var_stock, type="BG", lags.pt = 52, lags.bg=52)
        if(1/roots(var_stock)[[1]] > 1 & 1/roots(var_stock)[[2]] > 1){
            roots_stock <- "stable"
        } else {
            roots_stock <- "not stable"
        }



        # Causality from first column (sanctions) to second (breaks)
        wt2_stock<-wald.test(
            b=coef(var_stock$varresult[[2]]),
            Sigma=vcov(var_stock$varresult[[2]]),
            Terms=c(seq(1, AIC_stock*2, 2)))
        stock_causality_p <- wt2_stock[["result"]][["chi2"]][["P"]]

        # Saving results
        results_df <- data.frame(
            item = c("cpi", "stock"),
            aic_lag = c(AIC_cpi, AIC_stock),
            max_int_order = c(cpi_max_ord, stock_max_ord),
            serial_ac_p = c(cpi_serial$serial$p.value, stock_serial$serial$p.value),
            root = c(roots_cpi, roots_stock),
            causality_p = c(cpi_causality_p, stock_causality_p )
        )
        dbWriteTable(con_target, tab, results_df, overwrite = TRUE)
    }, error = function(e) {
        cat("Error for category ", tab, ": ", conditionMessage(e), "\n")
        #next
    })
}

##### FINANCIAL SANCTIONS #####
for (tab in names(cpi_ts)){
    tryCatch({
        ##### CPI TY Causality #####
        # Check order of integration for the time series
        cpi_order_integration <- order_integration(
            merge(sanctions_fin_ts$total, cpi_ts[,tab]),
            max_order = 5)
        # Select max order of integration
        var_cpi_select <- VARselect(merge(sanctions_fin_ts$total, cpi_ts[,tab]),
                                    lag.max = 12,
                                    type = "both")
        # Selecting the lag for VAR
        AIC_cpi <- var_cpi_select$selection[[1]]
        cpi_max_ord <- max(cpi_order_integration$order_int)
        total_cpi_lag <- AIC_cpi + cpi_max_ord

        # Performing VAR
        var_cpi <- VAR(
            merge(sanctions_fin_ts$total, cpi_ts[,tab]),
            p=total_cpi_lag,
            type="both")


        cpi_serial <- serial.test(var_cpi, type="BG", lags.pt = 52, lags.bg=52)
        if(1/roots(var_cpi)[[1]] > 1 || 1/roots(var_cpi)[[2]] > 1){
            roots_cpi <- "stable"
        } else {
            roots_cpi <- "not stable"
        }


        # Causality from first column (sanctions) to second (breaks)
        wt2_cpi<-wald.test(
            b=coef(var_cpi$varresult[[2]]),
            Sigma=vcov(var_cpi$varresult[[2]]),
            Terms=c(seq(1, AIC_cpi*2, 2)))
        cpi_causality_p <- wt2_cpi[["result"]][["chi2"]][["P"]]
        ##### Stock TY Causality #####

        stock_order_integration <- order_integration(
            merge(sanctions_fin_ts$total, stock_ts[,tab]),
            max_order = 5)

        var_stock_select <- VARselect(
            merge(sanctions_fin_ts$total, stock_ts[,tab]),
            lag.max = 12,
            type = "both")

        AIC_stock <- var_stock_select$selection[[1]]
        stock_max_ord <- max(stock_order_integration$order_int)
        total_stock_lag <- AIC_stock + cpi_max_ord

        var_stock <- VAR(merge(sanctions_fin_ts$total, stock_ts[,tab]),
                         p=total_stock_lag,
                         type="both")


        stock_serial <- serial.test(var_stock, type="BG", lags.pt = 52, lags.bg=52)
        if(1/roots(var_stock)[[1]] > 1 & 1/roots(var_stock)[[2]] > 1){
            roots_stock <- "stable"
        } else {
            roots_stock <- "not stable"
        }



        # Causality from first column (sanctions) to second (breaks)
        wt2_stock<-wald.test(
            b=coef(var_stock$varresult[[2]]),
            Sigma=vcov(var_stock$varresult[[2]]),
            Terms=c(seq(1, AIC_stock*2, 2)))
        stock_causality_p <- wt2_stock[["result"]][["chi2"]][["P"]]

        # Saving results
        results_df <- data.frame(
            item = c("cpi", "stock"),
            aic_lag = c(AIC_cpi, AIC_stock),
            max_int_order = c(cpi_max_ord, stock_max_ord),
            serial_ac_p = c(cpi_serial$serial$p.value, stock_serial$serial$p.value),
            root = c(roots_cpi, roots_stock),
            causality_p = c(cpi_causality_p, stock_causality_p )
        )
        dbWriteTable(con_target_fin, tab, results_df, overwrite = TRUE)
    }, error = function(e) {
        cat("Error for category ", tab, ": ", conditionMessage(e), "\n")
        #next
    })
}

##### TRADE SANCTIONS #####
for (tab in names(cpi_ts)){
    tryCatch({
        ##### CPI TY Causality #####

        # Check order of integration for the time series
        cpi_order_integration <- order_integration(
            merge(sanctions_trade_ts$total, cpi_ts[,tab]),
            max_order = 5)
        # Select max order of integration
        var_cpi_select <- VARselect(merge(sanctions_trade_ts$total, cpi_ts[,tab]),
                                    lag.max = 12,
                                    type = "both")
        # Selecting the lag for VAR
        AIC_cpi <- var_cpi_select$selection[[1]]
        cpi_max_ord <- max(cpi_order_integration$order_int)
        total_cpi_lag <- AIC_cpi + cpi_max_ord

        # Performing VAR
        var_cpi <- VAR(
            merge(sanctions_trade_ts$total, cpi_ts[,tab]),
            p=total_cpi_lag,
            type="both")


        cpi_serial <- serial.test(var_cpi, type="BG", lags.pt = 52, lags.bg=52)
        if(1/roots(var_cpi)[[1]] > 1 || 1/roots(var_cpi)[[2]] > 1){
            roots_cpi <- "stable"
        } else {
            roots_cpi <- "not stable"
        }

        # Causality from first column (sanctions) to second (breaks)
        wt2_cpi<-wald.test(
            b=coef(var_cpi$varresult[[2]]),
            Sigma=vcov(var_cpi$varresult[[2]]),
            Terms=c(seq(1, AIC_cpi*2, 2)))
        cpi_causality_p <- wt2_cpi[["result"]][["chi2"]][["P"]]
        ##### Stock TY Causality #####
        stock_order_integration <- order_integration(
            merge(sanctions_trade_ts$total, stock_ts[,tab]),
            max_order = 5)

        var_stock_select <- VARselect(
            merge(sanctions_trade_ts$total, stock_ts[,tab]),
            lag.max = 12,
            type = "both")

        AIC_stock <- var_stock_select$selection[[1]]
        stock_max_ord <- max(stock_order_integration$order_int)
        total_stock_lag <- AIC_stock + cpi_max_ord

        var_stock <- VAR(merge(sanctions_trade_ts$total, stock_ts[,tab]),
                         p=total_stock_lag,
                         type="both")


        stock_serial <- serial.test(var_stock, type="BG", lags.pt = 52, lags.bg=52)
        if(1/roots(var_stock)[[1]] > 1 & 1/roots(var_stock)[[2]] > 1){
            roots_stock <- "stable"
        } else {
            roots_stock <- "not stable"
        }



        # Causality from first column (sanctions) to second (breaks)
        wt2_stock<-wald.test(
            b=coef(var_stock$varresult[[2]]),
            Sigma=vcov(var_stock$varresult[[2]]),
            Terms=c(seq(1, AIC_stock*2, 2)))
        stock_causality_p <- wt2_stock[["result"]][["chi2"]][["P"]]

        # Saving results
        results_df <- data.frame(
            item = c("cpi", "stock"),
            aic_lag = c(AIC_cpi, AIC_stock),
            max_int_order = c(cpi_max_ord, stock_max_ord),
            serial_ac_p = c(cpi_serial$serial$p.value, stock_serial$serial$p.value),
            root = c(roots_cpi, roots_stock),
            causality_p = c(cpi_causality_p, stock_causality_p )
        )
        dbWriteTable(con_target_trade, tab, results_df, overwrite = TRUE)
    }, error = function(e) {
        cat("Error for category ", tab, ": ", conditionMessage(e), "\n")
        #next
    })
}


# Disconnect from DB
dbDisconnect(conn = con_cpi)
dbDisconnect(conn = con_stock)
dbDisconnect(conn = con_target)
dbDisconnect(conn = con_target_fin)
dbDisconnect(conn = con_target_trade)
