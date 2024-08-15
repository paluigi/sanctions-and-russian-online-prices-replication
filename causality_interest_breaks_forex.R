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


# Get Interest rate data
int_df <-  read_excel("official_data/ruonia.xlsx",
                      col_types = c("date", "numeric", "skip",
                                    "skip", "skip", "skip", "skip", "skip",
                                    "skip", "skip", "skip"))

# Manipulate interest rate data ans get structural break probability via BEAST
int_grouped_df <- int_df %>%
    mutate(DT = as.Date(DT)) %>%
    filter(DT >= as.Date("2021-02-14")) %>%
    filter(DT < as.Date("2022-10-10")) %>% # Removing data after CPI cutoff...to be updated over time
    group_by(week = cut(DT, "week")) %>%
    mutate(date = as.Date(week) + 6)  %>%  # Move reference to the end of the week
    mutate(rate=mean(ruo, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(date, rate) %>%
    arrange(date)

int_all_ts <- as.xts(int_grouped_df$rate, order.by = int_grouped_df$date)
names(int_all_ts) <- "rate"
# add missing dates
int_all_ts <- merge(int_all_ts, seq.Date(as.Date("2021-02-21"), as.Date("2022-10-09"), by = "week"))
int_all_ts <- na.fill(int_all_ts, "extend")



# Compute Beast algorithm
res_beast <- beast(int_all_ts, season = "none")
int_all_ts$cp_prob <- round(res_beast[["trend"]][["cpOccPr"]], n_dec)


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
    mutate(forex=mean(Close, na.rm = TRUE)) %>%
    ungroup() %>%
    distinct(date, forex)

forex_ts <- as.xts(forex_grouped_df$forex, order.by = forex_grouped_df$date)
names(forex_ts) <- "forex"
# Compute Beast algorithm
res_beast <- beast(forex_ts, season = "none")
forex_ts$cp_prob <- round(res_beast[["trend"]][["cpOccPr"]], n_dec)

##### INTEREST LEVEL #####
# Check order of integration for the time series
forex_order_integration <- order_integration(
    merge(int_all_ts$cp_prob, forex_ts$cp_prob),
    max_order = 5)
# Select max order of integration
var_forex_select <- VARselect(merge(int_all_ts$cp_prob, forex_ts$cp_prob),
                              lag.max = 23,
                              type = "both")
# Selecting the lag for VAR
AIC_forex <- var_forex_select$selection[[1]]
forex_max_ord <- max(forex_order_integration$order_int)
total_forex_lag <- AIC_forex + forex_max_ord

# Performing VAR
var_forex <- VAR(
    merge(int_all_ts$cp_prob, forex_ts$cp_prob),
    p=total_forex_lag,
    type="both")


forex_serial <- serial.test(var_forex, type="BG", lags.pt = 52, lags.bg=52)
if(1/roots(var_forex)[[1]] > 1 || 1/roots(var_forex)[[2]] > 1){
    roots_forex <- "stable"
} else {
    roots_forex <- "not stable"
}

# Causality from second column (breaks) to first (sanctions)
wt1_forex<-wald.test(
    b=coef(var_forex$varresult[[1]]),
    Sigma=vcov(var_forex$varresult[[1]]),
    Terms=c(seq(2, AIC_forex*2, 2)))
inverse_caus_p <- wt1_forex[["result"]][["chi2"]][["P"]]

# Causality from first column (sanctions) to second (breaks)
wt2_forex<-wald.test(
    b=coef(var_forex$varresult[[2]]),
    Sigma=vcov(var_forex$varresult[[2]]),
    Terms=c(seq(1, AIC_forex*2, 2)))
forex_causality_p <- wt2_forex[["result"]][["chi2"]][["P"]]

# Saving results
results_df <- data.frame(
    item = c("intbreaks_forex"),
    aic_lag = c(AIC_forex),
    max_int_order = c(forex_max_ord),
    serial_ac_p = c(forex_serial$serial$p.value),
    root = c(roots_forex),
    causality_p = c(forex_causality_p),
    inverse_causality_p = c(inverse_caus_p)
)



write_csv(results_df,
          file = file.path(data_path, "breaks_intrate_forex_causality.csv"))



