library(readr)
library(dplyr)
library(stringr)
library(tidyr)
library(DBI)
library(fuzzyjoin)

working_path <- file.path(getwd(), "llm")
data_path <- file.path(getwd(), "data")

categories <- c("5-3-2_smallappliances", "5-3-1_majorappliances", "5-5-2_smalltools", "5-5-1_majortools")
con <- dbConnect(RSQLite::SQLite(),
                     dbname = file.path(working_path, "product_classification.db"))


# Load data
brands <- read_csv(file.path(working_path, "brand_origin_list.csv"),
                   col_types = cols(notes = col_skip())) |>
    mutate(origin=case_when(
        country == "Russia" ~ "Domestic",
        country %in% c("China", "India", "Turkey", "Belarus", "Brazil", "Israel") ~ "Nonsanctioning",
        TRUE ~ "Sanctioning"
    ))

products <- dbReadTable(con, "products") |>
    filter(category %in% categories) |>
    separate(name, into = c("brand", "dropme"), sep = " - ", remove = FALSE, extra = "merge") |>
    select(-dropme) |>
    inner_join(brands, by="brand")

# Check
#sort(unique(products$brand)) == sort(unique(brands$brand))

# Load original data
prices <- data.frame()

for (cat in categories) {
    temp_df <- read_csv(file.path(data_path, paste0(cat, ".csv")),
                        col_types = cols(date = col_date(format = "%Y-%m-%d")))
    prices <- bind_rows(prices, temp_df)
}

# cleanup
rm(cat, temp_df)

# Issue in matching >> different extractions/encodings, some product does not match perfectly
# Performing exact match first, and fuzzy matching on residuals
diff1 <- sort(setdiff(unique(prices$name), unique(products$name) )) # Elements in prices but not in products

exact_prices <- prices %>%
    filter(!name %in% diff1) %>%
    left_join(., products, by="name")

fuzzy_prices <- prices %>%
    filter(name %in% diff1) %>%
    stringdist_left_join(., products, by = "name", distance_col = "distance", max_dist = 24) %>% # Increase distance to match all
    mutate(name=name.x) %>%
    select(-c(name.x, name.y)) %>%
    group_by(name, date) %>%
    slice(which.min(distance)) %>%
    ungroup() %>%
    select(-distance)

# Merge again together
prices <- bind_rows(exact_prices, fuzzy_prices) |>
    mutate(name=factor(name)) |>
    mutate(brand=factor(brand)) |>
    mutate(country=factor(country)) |>
    mutate(origin=factor(origin)) |>
    mutate(sanctions=factor(sanctions)) |>
    mutate(category=factor(category))

# Cleanup
rm(fuzzy_prices, exact_prices)

# Write by category in DB
for (cat in categories) {
    temp_df <- prices |> filter(category==cat) |> mutate(date=as.character(date))
    dbWriteTable(con, cat, temp_df, overwrite=TRUE)
}


# Disconnect from DB
dbDisconnect(conn = con)

