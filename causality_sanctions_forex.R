library(dplyr)
library(readxl)
library(xts)
library(fUnitRoots)
library(urca)
library(vars)
library(aod)
library(zoo)
library(tseries)
library(bootUR)
library(Rbeast)
library(purrr)
library(readr)


# Prevent scientific notation
options(scipen=999)
# Decimals to be saved
n_dec <- 7

# Connect to DB
data_path <- file.path(getwd(), "elaborations")


# Get sanctions data and forex data
sanctions_df <- read_excel("official_data/russia-sanction-timeline.xlsx",
                           col_types = c("date", "text", "text"))

forex_df <- read_csv("official_data/RUBUSD.csv",
                   col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                    Open = col_skip(), High = col_skip(),
                                    Low = col_skip(), `Adj Close` = col_skip(),
                                    Volume = col_skip()))

# Manipulate sanctions data
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
forex_ts$cp_prob <- round(res_beast[["trend"]][["cpOccPr"]], n_dec)

##### ALL SANCTIONS #####
# Check order of integration for the time series
forex_order_integration <- order_integration(
    merge(sanctions_all_ts$total, forex_ts$cp_prob),
    max_order = 5)
# Select max order of integration
var_forex_select <- VARselect(merge(sanctions_all_ts$total, forex_ts$cp_prob),
                            lag.max = 23,
                            type = "both")
# Selecting the lag for VAR
AIC_forex <- var_forex_select$selection[[1]]
forex_max_ord <- max(forex_order_integration$order_int)
total_forex_lag <- AIC_forex + forex_max_ord

# Performing VAR
var_forex <- VAR(
    merge(sanctions_all_ts$total, forex_ts$cp_prob),
    p=total_forex_lag,
    type="both")


forex_serial <- serial.test(var_forex, type="BG", lags.pt = 52, lags.bg=52)
if(1/roots(var_forex)[[1]] > 1 || 1/roots(var_forex)[[2]] > 1){
    roots_forex <- "stable"
} else {
    roots_forex <- "not stable"
}


# Causality from first column (sanctions) to second (breaks)
wt2_forex<-wald.test(
    b=coef(var_forex$varresult[[2]]),
    Sigma=vcov(var_forex$varresult[[2]]),
    Terms=c(seq(1, AIC_forex*2, 2)))
forex_causality_p <- wt2_forex[["result"]][["chi2"]][["P"]]

# Saving results
results1_df <- data.frame(
    item = c("forex_all"),
    aic_lag = c(AIC_forex),
    max_int_order = c(forex_max_ord),
    serial_ac_p = c(forex_serial$serial$p.value),
    root = c(roots_forex),
    causality_p = c(forex_causality_p)
)

##### FINANCIAL SANCTIONS #####
# Check order of integration for the time series
forex_order_integration <- order_integration(
    merge(sanctions_fin_ts$total, forex_ts$cp_prob),
    max_order = 5)
# Select max order of integration
var_forex_select <- VARselect(merge(sanctions_fin_ts$total, forex_ts$cp_prob),
                              lag.max = 12,
                              type = "both")
# Selecting the lag for VAR
AIC_forex <- var_forex_select$selection[[1]]
forex_max_ord <- max(forex_order_integration$order_int)
total_forex_lag <- AIC_forex + forex_max_ord

# Performing VAR
var_forex <- VAR(
    merge(sanctions_fin_ts$total, forex_ts$cp_prob),
    p=total_forex_lag,
    type="both")


forex_serial <- serial.test(var_forex, type="BG", lags.pt = 52, lags.bg=52)
if(1/roots(var_forex)[[1]] > 1 || 1/roots(var_forex)[[2]] > 1){
    roots_forex <- "stable"
} else {
    roots_forex <- "not stable"
}


# Causality from first column (sanctions) to second (breaks)
wt2_forex<-wald.test(
    b=coef(var_forex$varresult[[2]]),
    Sigma=vcov(var_forex$varresult[[2]]),
    Terms=c(seq(1, AIC_forex*2, 2)))
forex_causality_p <- wt2_forex[["result"]][["chi2"]][["P"]]

# Saving results
results2_df <- data.frame(
    item = c("forex_fin"),
    aic_lag = c(AIC_forex),
    max_int_order = c(forex_max_ord),
    serial_ac_p = c(forex_serial$serial$p.value),
    root = c(roots_forex),
    causality_p = c(forex_causality_p)
)



##### TRADE SANCTIONS #####
# Check order of integration for the time series
forex_order_integration <- order_integration(
    merge(sanctions_trade_ts$total, forex_ts$cp_prob),
    max_order = 5)
# Select max order of integration
var_forex_select <- VARselect(merge(sanctions_trade_ts$total, forex_ts$cp_prob),
                              lag.max = 12,
                              type = "both")
# Selecting the lag for VAR
AIC_forex <- var_forex_select$selection[[1]]
forex_max_ord <- max(forex_order_integration$order_int)
total_forex_lag <- AIC_forex + forex_max_ord

# Performing VAR
var_forex <- VAR(
    merge(sanctions_trade_ts$total, forex_ts$cp_prob),
    p=total_forex_lag,
    type="both")


forex_serial <- serial.test(var_forex, type="BG", lags.pt = 52, lags.bg=52)
if(1/roots(var_forex)[[1]] > 1 || 1/roots(var_forex)[[2]] > 1){
    roots_forex <- "stable"
} else {
    roots_forex <- "not stable"
}


# Causality from first column (sanctions) to second (breaks)
wt2_forex<-wald.test(
    b=coef(var_forex$varresult[[2]]),
    Sigma=vcov(var_forex$varresult[[2]]),
    Terms=c(seq(1, AIC_forex*2, 2)))
forex_causality_p <- wt2_forex[["result"]][["chi2"]][["P"]]

# Saving results
results3_df <- data.frame(
    item = c("forex_trade"),
    aic_lag = c(AIC_forex),
    max_int_order = c(forex_max_ord),
    serial_ac_p = c(forex_serial$serial$p.value),
    root = c(roots_forex),
    causality_p = c(forex_causality_p)
)


# Concatenate results and save as csv
results_df <- bind_rows(results1_df, results2_df, results3_df)

write_csv(results_df,
          file = file.path(data_path, "sanctions_forex_causality.csv"))



