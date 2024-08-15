library(dplyr)
library(purrr)
library(xts)
library(Rbeast)
library(DBI)
library(imputeTS)


# Prevent scientific notation
options(scipen=999)
# Decimals to be saved
n_dec <- 7

data_path <- file.path(getwd(), "elaborations")

# Flag for prices
flag <- "cpi"

con <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(data_path, paste0(flag, ".db")))

con_dest <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(data_path, paste0(flag, "_dest.db")))

# Perform BEAST on all tables and save back the results
for (table in dbListTables(con)) {
    df <- dbReadTable(con, table)
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    ts <- as.xts(df$index, order.by = df$date)
    # add missing dates
    ts <- merge(ts, seq.Date(min(df$date), max(df$date), by = "week"))
    # Compute Beast algorithm
    res_beast <- beast(ts, season = "none")
    # Save trend
    ts$trend <- round(res_beast[["trend"]][["slp"]], n_dec)
    ts <-  data.frame(date=index(ts), coredata(ts))
    colnames(ts) <- c("date", "cpi", "trend")
    ts <- ts %>%
        mutate(date = as.Date(date)) %>%
        mutate(cpi = round(na_interpolation(cpi, option="linear"), n_dec)) %>%
        mutate(cpi_pre = ifelse(date < as.Date("2022-02-24"), cpi, NA)) %>%
        mutate(trend_pre = ifelse(date < as.Date("2022-02-24"), trend, NA))
    # Impute trend for cpi values after war start
    mean_trend_pre <- mean(ts$trend, na.rm = TRUE)
    projection <- ts %>%
        group_by(grp = cumsum(!is.na(cpi_pre)), .add = TRUE) %>%
        ungroup() %>%
        filter(grp == max(grp)) %>%
        mutate(cpi_post = round(accumulate(cpi_pre, ~ .x + mean_trend_pre), n_dec)) %>%
        mutate(cpi_diff = round(cpi - cpi_post, n_dec)) %>%
        select(c(date, cpi_post, cpi_diff))

    ts <- merge(ts, projection, by="date", all=TRUE) %>%
        mutate(cpi_diff = na_replace(cpi_diff, fill=0)) %>%
        mutate(date = as.character.Date(date))

    dbWriteTable(con_dest, table, ts, overwrite = TRUE)

    # also perform beast on official data
    ts <- as.xts(df$official, order.by = df$date)
    # remove empty lines
    ts <- na.omit(ts)
    # Compute Beast algorithm
    res_beast <- beast(ts, season = "none")
    # Save trend
    ts <-  data.frame(date=index(ts), coredata(ts))
    ts$trend <- round(res_beast[["trend"]][["slp"]], n_dec)
    colnames(ts) <- c("date", "cpi", "trend")
    ts <- ts %>%
        mutate(date = as.Date(date)) %>%
        #mutate(cpi = round(na_interpolation(cpi, option="linear"), n_dec)) %>%
        mutate(cpi_pre = ifelse(date < as.Date("2022-02-24"), cpi, NA)) %>%
        mutate(trend_pre = ifelse(date < as.Date("2022-02-24"), trend, NA))
    # Impute trend for cpi values after war start
    mean_trend_pre <- mean(ts$trend, na.rm = TRUE)
    projection <- ts %>%
        group_by(grp = cumsum(!is.na(cpi_pre)), .add = TRUE) %>%
        ungroup() %>%
        filter(grp == max(grp)) %>%
        mutate(cpi_post = round(accumulate(cpi_pre, ~ .x + mean_trend_pre), n_dec)) %>%
        mutate(cpi_diff = round(cpi - cpi_post, n_dec)) %>%
        select(c(date, cpi_post, cpi_diff))

    ts <- merge(ts, projection, by="date", all=TRUE) %>%
        mutate(cpi_diff = na_replace(cpi_diff, fill=0)) %>%
        mutate(date = as.character.Date(date))

    dbWriteTable(con_dest, paste0(table,"_official"), ts, overwrite = TRUE)
}

# Disconnect from DB
dbDisconnect(conn = con)
dbDisconnect(conn = con_dest)
dbDisconnect(conn = con_off)


