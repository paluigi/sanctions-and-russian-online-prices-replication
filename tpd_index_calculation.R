library(readr)
library(dplyr)
library(biglm)
library(DBI)

# Variables
data_path <- file.path(getwd(), "data")
results_path <- file.path(getwd(), "elaborations")

# Flag for prices or stock
#flag <- "cpi"
flag <- "stock"

con <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(results_path, paste0(flag, ".db")))

# Get categories from CSV file
coicop1999_categories <- read_csv("coicop1999_categories.csv")

for (f in coicop1999_categories$category) {
    # Read file
    file_path <- file.path(data_path, paste0(f, ".csv"))
    data_df <- read_csv(file_path)
    # Data as factors
    data_df$name <- as.factor(data_df$name)
    data_df$date_cat <- as.factor(data_df$date)
    data_df$date_cat <- relevel(data_df$date_cat, ref = "2021-02-28")
    data_df <- as.data.frame(data_df)
    # Switch for prices and stock
    if (flag == "cpi") {
        data_df$log_target <- log(data_df$price)
    } else {
        data_df$log_target <- log(data_df$stock)
    }
    # Remove infinites and NAs. Infinites are bound to zero
    data_df[sapply(data_df, is.infinite)] <- 0
    data_df <- data_df[complete.cases(data_df), ]
    # Calculate model
    model <- biglm(log_target ~ date_cat + name, data = data_df)
    coeff <- coef(model)
    # Drop the model to recover memory
    rm(model)
    gc()
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
    # Get the name of category as DB table name and save to DB
    dbWriteTable(con, f, result_df)
    # Saving to CSV, obsolete
    #output_path <- file.path(results_path, paste0(flag, "_", f))
    #write_csv(result_df, output_path)
}

# Disconnect from DB
dbDisconnect(conn = con)
