library(DBI)
library(dplyr)
library(tidyr)
library(readr)
library(xtable)


# Prevent scientific notation
options(scipen=999)

# Define output file
output_file <- "results_files/simple_correlation.txt"


tpd_db <- file.path(getwd(), "elaborations", "cpi.db")

con_tpd <- dbConnect(RSQLite::SQLite(),
                     dbname = tpd_db)

categories_pretty <- read_csv("coicop1999_categories_pretty.csv")

results_df <- data.frame()
for (table in dbListTables(con_tpd)) {
    df <-  dbReadTable(con_tpd, table ) |>
        select(date, index, official) %>%
        mutate(date=as.Date(date)) |>
        drop_na()
    x <- cor.test(df$index, df$official)
    new_res <- data.frame(
        category=table,
        correlation=x$estimate,
        pvalue=x$p.value
    )
    results_df <- bind_rows(results_df, new_res)

}

results_df <- merge(results_df, categories_pretty, by="category") |>
    select(pretty, correlation, pvalue) |>
    arrange(pretty)

print(xtable(results_df), file = output_file, compress = FALSE, include.rownames=FALSE)


sum(results_df$correlation > 0.7)
sum(results_df$correlation > 0.9)
sum(results_df$pvalue > 0.05)

# Disconnect from DB
dbDisconnect(conn = con_tpd)
