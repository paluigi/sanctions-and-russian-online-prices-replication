library(readr)
library(dplyr)
library(stringr)
library(lubridate)
library(tidyr)
library(DBI)


tpd_index <- function (data_df) {
    data_df$name <- as.factor(data_df$product)
    data_df$date_cat <- as.factor(data_df$date)
    data_df$date_cat <- relevel(data_df$date_cat, ref = "2021-02-28")
    data_df <- as.data.frame(data_df)
    data_df$log_target <- log(data_df$price)
    # Remove infinites and NAs. Infinites are bound to zero
    data_df[sapply(data_df, is.infinite)] <- 0
    data_df <- data_df[complete.cases(data_df), ]
    # Calculate model
    model <- lm(log_target ~ date_cat + name, data = data_df)
    coeff <- coef(model)
    # Elaborate results
    result_df <- as.data.frame(coeff[grep("^date_cat", names(coeff))])
    result_df$date <- rownames(result_df)
    colnames(result_df) <- c("index", "date")
    result_df$date <- gsub("date_cat", "", as.character(result_df$date))
    result_df$index <- (exp(result_df$index) * 100)
    # Add base value for Feb 28th 2021
    result_df <- add_row(result_df,
                         index = 100,
                         date = "2021-02-28",
                         .before = 2)
    return(result_df)
}

tprd_index <- function (data_df) {
    data_df$name <- as.factor(data_df$product)
    data_df$date_cat <- as.factor(data_df$date)
    data_df$store_id <- as.factor(data_df$store_id)
    data_df$date_cat <- relevel(data_df$date_cat, ref = "2021-02-28")
    data_df <- as.data.frame(data_df)
    data_df$log_target <- log(data_df$price)
    # Remove infinites and NAs. Infinites are bound to zero
    data_df[sapply(data_df, is.infinite)] <- 0
    data_df <- data_df[complete.cases(data_df), ]
    # Calculate model
    model <- lm(log_target ~ date_cat + name + store_id, data = data_df)
    coeff <- coef(model)
    # Elaborate results
    result_df <- as.data.frame(coeff[grep("^date_cat", names(coeff))])
    result_df$date <- rownames(result_df)
    colnames(result_df) <- c("index", "date")
    result_df$date <- gsub("date_cat", "", as.character(result_df$date))
    result_df$index <- (exp(result_df$index) * 100)
    # Add base value for Feb 28th 2021
    result_df <- add_row(result_df,
                         index = 100,
                         date = "2021-02-28",
                         .before = 2)
    return(result_df)
}

# setup
results_path <- file.path(getwd(), "italian_match_products")

con_w <- dbConnect(RSQLite::SQLite(),
                   dbname = file.path(results_path, paste0("matching_weekly.db")))

# Load data
russia <- read_csv("italian_match_products/russia_weekly.csv",
                        col_types = cols(date = col_date(format = "%Y-%m-%d")))

italy <- read_csv("italian_match_products/italy_weekly.csv",
                       col_types = cols(date = col_date(format = "%Y-%m-%d"),
                                        size = col_skip(), unit = col_skip(),
                                        lat = col_skip(), lon = col_skip(),
                                        chain = col_skip()))

rub_eur <- read_csv("official_data/RUBEUR.csv",
                    col_types = cols(Date = col_date(format = "%m/%d/%y"),
                                     Open = col_skip(), High = col_skip(),
                                     Low = col_skip())) |>
    mutate(date=Date) |>
    mutate(forex=Close) |>
    select(date, forex)


rub_usd <- read_csv("official_data/RUBUSD.csv",
                    col_types = cols(Date = col_date(format = "%Y-%m-%d"),
                                     Open = col_skip(), High = col_skip(),
                                     Low = col_skip(), Close = col_skip(),
                                     Volume = col_skip())) |>
    mutate(date=Date) |>
    mutate(forex=`Adj Close`) |>
    select(date, forex)


# Cut dates, rearrange brands and select Lazio only for comparison
start_date <- as.Date("2021-02-21") # Inclusive
end_date <- as.Date("2022-10-09") # Inclusive



# Fill forex with holidays and fill forward
complete_dates <- data.frame(
    date = seq(min(rub_usd$date), max(rub_usd$date), by = "day")
)

forex <- complete_dates |>
    left_join(rub_usd, by = "date") |>
    mutate(week = ceiling_date(date, unit="week", week_start = 1) -1) |> # Need to remove 1 to get the last day of the week
    group_by(week) |>
    mutate(w_forex = mean(forex, na.rm=TRUE)) |>
    ungroup() |>
    fill(forex, .direction="down") |>
    filter(date>= start_date) |>
    filter(date<= end_date)

# Check for matching brands
sort(unique(italy$brand)) == sort(unique(russia$brand))

# Loop to calculate weekly index per brand, with variations, and save to db
for (b in unique(russia$brand)) {
    print(b)
    italy_temp <- italy |>
        filter(brand == b) |>
        distinct(product, store_id, week, .keep_all=TRUE) |>
        mutate(price=w_price) |>
        mutate(date=week) |>
        select(product, date, price, store_id)
    print(paste0(b, " - italian TPD"))
    italy_tpd <- tpd_index(italy_temp)
    colnames(italy_tpd) <- c("italy_tpd", "date")
    print(paste0(b, " - italian TPRD"))
    italy_tprd <- tprd_index(italy_temp)
    colnames(italy_tprd) <- c("italy_tprd", "date")
    russia_temp <- russia |>
        filter(brand == b) |>
        distinct(product, week, .keep_all=TRUE) |>
        mutate(price=w_price) |>
        mutate(date=week) |>
        select(product, date, price)
    print(paste0(b, " - russian TPD"))
    russia_tpd <- tpd_index(russia_temp)
    colnames(russia_tpd) <- c("russia", "date")
    russia_temp <- russia_temp %>%
        inner_join(., (forex |> distinct(week, .keep_all=TRUE) |> mutate(date=week) |> mutate(forex=w_forex) |> select(date, forex)), by="date") %>%
        mutate(price=price/forex) %>%
        select(product, date, price)
    print(paste0(b, " - russian TPD in Eur"))
    russia_tpd_eur <- tpd_index(russia_temp)
    colnames(russia_tpd_eur) <- c("russia_usd", "date")
    result_df <- italy_tpd %>%
        merge(., italy_tprd, by="date", all=TRUE)%>%
        merge(., russia_tpd, by="date", all=TRUE)%>%
        merge(., russia_tpd_eur, by="date", all=TRUE)
    dbWriteTable(con_w, b, result_df)
}


# Disconnect from DB
dbDisconnect(conn = con_w)

