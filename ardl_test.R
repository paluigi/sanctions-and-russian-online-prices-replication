library(ARDL)
library(dplyr)
library(DBI)
library(imputeTS)


# Prevent scientific notation
options(scipen=999)
# Decimals to be saved
n_dec <- 4

# War start
war_start <- as.Date("2022-02-24", format = "%Y-%m-%d")

# Connect to DB
data_path <- file.path(getwd(), "elaborations")
origin <- "cpi_imputed"
target <- "ardl_tests"



con_origin <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(origin, ".db")))

con_target <- dbConnect(RSQLite::SQLite(),
                        dbname = file.path(data_path, paste0(target, ".db")))


# List to save results for each table
results_df <- data.frame()


# For each table:
# - Calculate ARDL cointegration test between index and official
# - Save back the results to target DB

for (table in dbListTables(con_origin)) {
    df <- dbReadTable(con_origin, table)
    df$date <- as.Date(df$date, format = "%Y-%m-%d")
    df <- df %>%
        filter(!is.na(official)) %>%
        mutate(index_complete = na_kalman(index)) %>%
        mutate(diff = index_complete - official)

    df_pre <- df %>% filter(date < war_start)
    df_post <- df %>% filter(date > war_start)

    # Cointegration testing
    # All periods
    all_model <- ardl(index_complete ~ official, data= df, order = 1)
    all_f_test <- bounds_f_test(all_model, case=3)
    all_pval <- all_f_test$p.value
    # Pre war
    pre_model <- ardl(index_complete ~  official, data= df_pre, order = 1)
    pre_f_test <- bounds_f_test(pre_model, case=3)
    pre_pval <- pre_f_test$p.value
    # After war start
    post_model <- ardl(index_complete ~ official, data= df_post, order = 1)
    post_f_test <- bounds_f_test(post_model, case=3)
    post_pval <- post_f_test$p.value

    # Results in one object
    results <- data.frame(
        table = table,
        all = all_pval,
        pre = pre_pval,
        post = post_pval
    )

    colnames(results) <- c("table", "all", "pre", "post")

    results_df <- bind_rows(results_df, results)
    # Revert date to character for saving
    dbWriteTable(con_target, table, results, overwrite = TRUE)
}

dbWriteTable(con_target, "summary", results_df, overwrite = TRUE)

# Disconnect from DB
dbDisconnect(conn = con_origin)
dbDisconnect(conn = con_target)

sum(results_df$all < 0.05)
sum(results_df$pre < 0.05)
sum(results_df$post < 0.05)
