library(PriceIndices)
library(DBI)
library(dplyr)

data_db <- file.path(getwd(), "rosstat_procedure", "rosstat.db")
dest_db <- file.path(getwd(), "rosstat_procedure", "r_cpi.db")
con_data <- dbConnect(RSQLite::SQLite(),
                      dbname = data_db)
con_res <- dbConnect(RSQLite::SQLite(),
                      dbname = dest_db)

month_labels <- c("2021-02-28", "2021-03-28", "2021-04-25", "2021-05-30",
                  "2021-06-27", "2021-07-25", "2021-08-29", "2021-09-26",
                  "2021-10-31", "2021-11-28", "2021-12-26", "2022-01-30",
                  "2022-02-27", "2022-03-27", "2022-04-24", "2022-05-29",
                  "2022-06-26", "2022-07-31", "2022-08-28", "2022-09-25")

for (table in dbListTables(con_data)) {
    df <- dbReadTable(con_data, table) |>
        mutate(time=as.Date(date)) |>
        filter(time<as.Date("2022-10-01")) |>
        mutate(prices=price) |>
        mutate(prodID=factor(product_id)) |>
        mutate(quantities=1) |>
        select(time, prices, prodID, quantities)

    months_to_remove <- c()
    # June 2021 imputation
    if (
        nrow(df |>
             filter(time < as.Date("2021-07-01")) |>
             filter(time > as.Date("2021-06-01"))
        ) == 0
    ) {
        impute_df <- df |>
            filter(time < as.Date("2021-06-01")) |>
            filter(time > as.Date("2021-05-01")) |>
            mutate(time=time+30)
        df <- bind_rows(df, impute_df)
        rm(impute_df)
        months_to_remove <- append(months_to_remove, "2021-06-27")
    }

    # August 2021 imputation
    if (
        nrow(df |>
             filter(time < as.Date("2021-09-01")) |>
             filter(time > as.Date("2021-08-01"))
        ) == 0
    ) {
        impute_df <- df |>
            filter(time < as.Date("2021-08-01")) |>
            filter(time > as.Date("2021-07-01")) |>
            mutate(time=time+30)
        df <- bind_rows(df, impute_df)
        rm(impute_df)
        months_to_remove <- append(months_to_remove, "2021-08-29")
    }

    # March 2022 imputation
    if (
        nrow(df |>
             filter(time < as.Date("2022-04-01")) |>
             filter(time > as.Date("2022-03-01"))
        ) == 0
    ) {
        impute_df <- df |>
            filter(time < as.Date("2022-03-01")) |>
            filter(time > as.Date("2022-02-01")) |>
            mutate(time=time+30)
        df <- bind_rows(df, impute_df)
        rm(impute_df)
        months_to_remove <- append(months_to_remove, "2022-03-27")
    }

    # June 2022 imputation
    if (
        nrow(df |>
             filter(time < as.Date("2022-07-01")) |>
             filter(time > as.Date("2022-06-01"))
        ) == 0
    ) {
        impute_df <- df |>
            filter(time < as.Date("2022-06-01")) |>
            filter(time > as.Date("2022-05-01")) |>
            mutate(time=time+30)
        df <- bind_rows(df, impute_df)
        rm(impute_df)
        months_to_remove <- append(months_to_remove, "2022-06-26")
    }

    sort(unique(df$time))

    results <- chlaspeyres(df, "2021-02", "2022-09", interval = TRUE)

    results_df <- data.frame(date=month_labels,
                             laspeyr=results) |>
        filter(!date %in% months_to_remove) |>
        mutate(laspeyr=laspeyr*100)

    dbWriteTable(con_res, table, results_df, overwrite=TRUE)
}

# Disconnect from DB
dbDisconnect(conn = con_data)
dbDisconnect(conn = con_res)
