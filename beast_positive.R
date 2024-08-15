library(dplyr)
library(xts)
library(Rbeast)
library(DBI)

# Prevent scientific notation
options(scipen=999)
# Decimals to be saved
n_dec <- 7

data_path <- file.path(getwd(), "elaborations")

# Flag for prices
flag <- "cpi"

con <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(data_path, paste0(flag, ".db")))

con_target <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(data_path, paste0(flag, "_positive.db")))

# Perform BEAST on all tables and save back the results
for (table in dbListTables(con)) {
    df <- dbReadTable(con, table)
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    ts <- as.xts(df$index, order.by = df$date)
    # add missing dates
    ts <- merge(ts, seq.Date(min(df$date), max(df$date), by = "week"))
    # Compute Beast algorithm
    res_beast <- beast(ts, season = "none")
    # Setup dataframe for saving
    ts$cp_prob <- round(res_beast[["trend"]][["pos_cpOccPr"]], n_dec)
    ts$slope_pos <- round(res_beast[["trend"]][["slpSgnPosPr"]], n_dec)
    ts$slope_zero <- round(res_beast[["trend"]][["slpSgnZeroPr"]], n_dec)
    ts <-  data.frame(date=index(ts), coredata(ts))
    colnames(ts) <- c("date", "index", "cp_prob", "slope_pos", "slope_zero")
    ts$date <- as.character.Date(ts$date)
    dbWriteTable(con_target, table, ts, overwrite = TRUE)
}

# Disconnect from DB
dbDisconnect(conn = con)
dbDisconnect(conn = con_target)
