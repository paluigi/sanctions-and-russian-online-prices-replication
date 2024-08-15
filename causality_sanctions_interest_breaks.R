library(dplyr)
library(readxl)
library(readr)
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
# Decimals to be saved
n_dec <- 7

# Connect to DB
data_path <- file.path(getwd(), "elaborations")

target <- "causality_sactions_interest_breaks"


con_target <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(target, ".db")))


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

##### ALL SANCTIONS - INT BREAKS #####
##### CPI TY Causality #####
# Check order of integration for the time series
cpi_order_integration <- order_integration(
    merge(sanctions_all_ts$total, int_all_ts$cp_prob),
    max_order = 5)
# Select max order of integration
var_cpi_select <- VARselect(merge(sanctions_all_ts$total, int_all_ts$cp_prob),
                            lag.max = 12,
                            type = "both")
# Selecting the lag for VAR
AIC_cpi <- var_cpi_select$selection[[1]]
cpi_max_ord <- max(cpi_order_integration$order_int)
total_cpi_lag <- AIC_cpi + cpi_max_ord

# Performing VAR
var_cpi <- VAR(
    merge(sanctions_all_ts$total, int_all_ts$cp_prob),
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


# Saving results
results_df <- data.frame(
    item = c("cpi"),
    aic_lag = c(AIC_cpi),
    max_int_order = c(cpi_max_ord),
    serial_ac_p = c(cpi_serial$serial$p.value),
    root = c(roots_cpi),
    causality_p = c(cpi_causality_p)
)
dbWriteTable(con_target, "all_sanctions_int_breaks", results_df, overwrite = TRUE)


##### FINANCIAL SANCTIONS - INT BREAKS#####

##### CPI TY Causality #####
# Check order of integration for the time series
cpi_order_integration <- order_integration(
    merge(sanctions_fin_ts$total, int_all_ts$cp_prob),
    max_order = 5)
# Select max order of integration
var_cpi_select <- VARselect(merge(sanctions_fin_ts$total, int_all_ts$cp_prob),
                            lag.max = 12,
                            type = "both")
# Selecting the lag for VAR
AIC_cpi <- var_cpi_select$selection[[1]]
cpi_max_ord <- max(cpi_order_integration$order_int)
total_cpi_lag <- AIC_cpi + cpi_max_ord

# Performing VAR
var_cpi <- VAR(
    merge(sanctions_fin_ts$total, int_all_ts$cp_prob),
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


# Saving results
results_df <- data.frame(
    item = c("cpi"),
    aic_lag = c(AIC_cpi),
    max_int_order = c(cpi_max_ord),
    serial_ac_p = c(cpi_serial$serial$p.value),
    root = c(roots_cpi),
    causality_p = c(cpi_causality_p)
)
dbWriteTable(con_target, "fin_sanctions_int_breaks", results_df, overwrite = TRUE)


##### TRADE SANCTIONS - INT BREAKS #####
##### CPI TY Causality #####

# Check order of integration for the time series
cpi_order_integration <- order_integration(
    merge(sanctions_trade_ts$total, int_all_ts$cp_prob),
    max_order = 5)
# Select max order of integration
var_cpi_select <- VARselect(merge(sanctions_trade_ts$total, int_all_ts$cp_prob),
                            lag.max = 12,
                            type = "both")
# Selecting the lag for VAR
AIC_cpi <- var_cpi_select$selection[[1]]
cpi_max_ord <- max(cpi_order_integration$order_int)
total_cpi_lag <- AIC_cpi + cpi_max_ord

# Performing VAR
var_cpi <- VAR(
    merge(sanctions_trade_ts$total, int_all_ts$cp_prob),
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

# Saving results
results_df <- data.frame(
    item = c("cpi"),
    aic_lag = c(AIC_cpi),
    max_int_order = c(cpi_max_ord),
    serial_ac_p = c(cpi_serial$serial$p.value),
    root = c(roots_cpi),
    causality_p = c(cpi_causality_p)
)
dbWriteTable(con_target, "tra_sanctions_int_breaks", results_df, overwrite = TRUE)





# Disconnect from DB
dbDisconnect(conn = con_target)

