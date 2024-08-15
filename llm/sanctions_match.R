library(haven)
library(readxl)
library(dplyr)
library(magrittr)
library(stringr)


rm(list=ls(all.names=TRUE))
# Read data
hs <- read_excel("llm/harmonized-system-ru.xlsx",
                                   col_types = c("skip", "text", "text",
                                                 "skip", "numeric"))


sanctions <- read_dta("llm/EU_sanctions_HS6.dta")   # This file belongs to the authors
                                                    # of the paper: The Eurasian Roundabout: Trade
                                                    # Flows Into Russia Through the Caucasus and Central Asia.
                                                    # Maxim Chupilkin, Beata Javorcik, and Alexander Plekhanov
                                                    # and it is not included in the replication package

sanctions <- sanctions |>
    select(Code, Date, EU_sanction, partially_exempt, Maxprice, luxury)


sanctions_desc <- sanctions %>%
    merge(., hs, by.x="Code", by.y="hscode", all=FALSE) %>%
    select(-c(level))

# Fix the missing matches
missing_desc <- setdiff(sanctions$Code, hs$hscode)

# First round, remove last digit from HS codes and keep first description
missing_sanctions <- sanctions |>
    filter(Code %in% missing_desc) |>
    mutate(code5= str_sub(Code, end=-2)) |>
    mutate(code4= str_sub(Code, end=-3))

hs_fill5 <- hs |>
    mutate(hs5 = str_sub(hscode, end=-2)) |>
    distinct(hs5, .keep_all = TRUE) |>
    filter(level==6) |>
    filter(hs5 %in% missing_sanctions$code5)

sanctions_rec1 <- missing_sanctions %>%
    merge(., hs_fill5, by.x = "code5", by.y = "hs5") %>%
    select(-c(code5, code4, hscode, level))

# Check duplicate descriptions for possible inconsistencies in sanctions coding
temp_sanctions <- rbind(sanctions_desc, sanctions_rec1)

dupl_check <- temp_sanctions |>
    select(description, Code, EU_sanction) |>
    filter(duplicated(description) | duplicated(description, fromLast = TRUE)) |> # This way includes both first and last duplicated row
    arrange(Code)

# Manual modification for two problematic codings:
# 1. Tobacco products: The sanctioned ones are only a subset. In any case, there is no tobacco product in our data
# Code all duplicated tabacco products as sanctioned by removing the ones marked as non-sanction
# 2. Human blood derivatives The sanctioned ones are only a subset. In any case, there is no Human blood derivative product in our data
# Code all duplicated Human blood derivative products as sanctioned

temp_sanctions <- temp_sanctions |>
    filter(!Code %in% c("240391", "300212"))


# Second round, remove two digits from HS codes and keep first description
still_miss <- setdiff(missing_sanctions$Code, sanctions_rec1$Code)

missing_sanctions2 <- missing_sanctions |>
    filter(Code %in% still_miss)

hs_fill4 <- hs |>
    mutate(hs4 = str_sub(hscode, end=-3)) |>
    distinct(hs4, .keep_all = TRUE) |>
    filter(level==6) |>
    filter(hs4 %in% missing_sanctions2$code4)


sanctions_rec2 <- missing_sanctions2 %>%
    merge(., hs_fill4, by.x = "code4", by.y = "hs4") %>%
    select(-c(code5, code4, hscode, level))

# Few items still missing...
still_miss2 <- setdiff(missing_sanctions2$Code, sanctions_rec2$Code)
# One ("255734") belongs to SALT; SULPHUR; EARTHS AND STONE; PLASTERING MATERIALS, LIME AND CEMENT
# Two ("810720" "810730") belongs to OTHER BASE METALS; CERMETS; ARTICLES THEREOF
# Four ("880310" "880320" "880330" "880390") belongs to AIRCRAFT, SPACECRAFT, AND PARTS THEREOF
# It's unlikely that those items are present in our data, hence we drop them from our analysis

# Combine all data in a single dataframe, check for duplicate descriptions and save to CSV
all_desc <- rbind(temp_sanctions, sanctions_rec2)

dupl_check <- all_desc |>
    select(description, Code, EU_sanction) |>
    filter(duplicated(description) | duplicated(description, fromLast = TRUE)) |> # This way includes both first and last duplicated row
    arrange(Code)

# Manual modification for  problematic codings:
# 1. Copper ore: The sanctioned ones are only a subset. In any case, there is no copper ore product in our data
# Code all duplicated copper ore products as sanctioned by removing the ones marked as non-sanction
# 2. Chemical derivatives The sanctioned ones are only a subset. In any case, there is no Chemical derivative product in our data
# Code all duplicated chemical derivative products as sanctioned
# 3. Human blood derivatives The sanctioned ones are only a subset. In any case, there is no Human blood derivative product in our data
# Code all duplicated Human blood derivative products as sanctioned
# 4. Surgical products The sanctioned ones are only a subset. In any case, there is no Surgical product in our data
# Code all duplicated surgical products as non-sanctioned
# 5. Foundry products The sanctioned ones are only a subset. In any case, there is no Foundry product in our data
# Code all duplicated Foundry products as non-sanctioned
# 6. Construction wood products The sanctioned ones are only a subset.
# Code all duplicated Construction wood  products as sanctioned

all_desc <- all_desc |>
    filter(!Code %in% c("260300", "290331", "300230", "300620", "382479", "441810")) |>
    distinct(description, .keep_all = TRUE)

# Remaining duplicated descriptions are concordant in the sanctions coding

write.csv(all_desc, file="llm/sanctions_hs6_desc.csv", row.names = FALSE)
