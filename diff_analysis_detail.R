library(dplyr)
library(purrr)
library(xts)
library(Rbeast)
library(DBI)
library(imputeTS)
library(ggplot2)
library(patchwork)
library(showtext)
library(imputeTS)
library(ggrepel)
library(readr)
library(stringr)
library(tidyr)


# Prevent scientific notation
options(scipen=999)
# Decimals to be saved
n_dec <- 7

# Add Book Antiqua font
font_add(family = "bookantiqua",
         regular = "/home/luigi/.local/share/fonts/book-antiqua.ttf")

load_showtext_fonts()
showtext_auto(enable = TRUE)

data_path <- file.path(getwd(), "elaborations")

# Flag for prices
flag <- "cpi"

con <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(data_path, paste0(flag, "_dest.db")))

# Mapping for category names
table_map <- read_csv("elaborations/table_map.csv")

# Get structural break probability for CPI
diff_df <- data.frame(date=as.Date("2021-02-21"))
off_df <- data.frame(date=as.Date("2021-02-28"))
for (table in dbListTables(con)) {
    if (endsWith(table, "_official")) {
        df <- dbReadTable(con, table)
        df$date <- as.Date(df$date, format = "%Y-%m-%d")
        df <- df %>% dplyr::select(date, cpi_diff)
        colnames(df) <- c("date", table)
        off_df <- full_join(off_df, df, by= "date")
    } else {
        df <- dbReadTable(con, table)
        df$date <- as.Date(df$date, format = "%Y-%m-%d")
        df <- df %>% dplyr::select(date, cpi_diff)
        colnames(df) <- c("date", table)
        diff_df <- full_join(diff_df, df, by= "date")
    }
}

# diff_df <- diff_df %>% filter(date > as.Date("2022-02-24")) %>%
#     mutate(cpi_diff= rowMeans(subset(., select=-c(date))))
#
# off_df <- off_df %>% filter(date > as.Date("2022-02-24")) %>%
#     mutate(cpi_diff= rowMeans(subset(., select=-c(date))))

# No need to calculate the mean
diff_df <- diff_df %>% filter(date > as.Date("2022-02-24"))

off_df <- off_df %>% filter(date > as.Date("2022-02-24"))

diff_rank <- colMeans(subset(diff_df, select=-c(date)))

off_rank <- colMeans(subset(off_df, select=-c(date)))

names(off_rank) <- gsub("_official", "", names(off_rank))

rank_corr <- cor.test(rank(-diff_rank), rank(-off_rank), method = "spearman")
num_corr <- cor.test(diff_rank, off_rank, method = "pearson")

sort(rank(-diff_rank), decreasing = FALSE)[1:10]
sort(rank(-off_rank), decreasing = FALSE)[1:10]

result_df <- data.frame(off_rank, diff_rank)

result_df$label <- table_map$name[match(rownames(result_df), table_map$table)]

result_df <- result_df %>%
    separate(label, into = c("code", "name"), sep = " ", remove = FALSE, extra="merge")

# Scatterplot
scatterplot <- ggplot(result_df, aes(x = off_rank, y = diff_rank)) +
    geom_vline(xintercept = mean(result_df$off_rank), color = "grey", linetype = "dashed") +
    geom_hline(yintercept = mean(result_df$diff_rank), color = "grey", linetype = "dashed") +
    geom_point(shape=18, size=1.5) +
    geom_text_repel(
        aes(label = code),
        vjust = +0.5,
        size = 4.5,
        segment.linetype = 2,
        segment.color = "black",
        segment.size = 0.2,
        family="bookantiqua"
        ) +
    theme_minimal() +
    theme(
        axis.line = element_line(color = "lightgrey"),
        panel.grid = element_blank(),
        axis.text.x = element_text(family = "bookantiqua"),
        axis.text.y = element_text(family = "bookantiqua"),
        axis.ticks = element_blank(),
        axis.title.x = element_text(family = "bookantiqua"),
        axis.title.y = element_text(family = "bookantiqua"),
        panel.border = element_blank(),
        plot.margin = margin(0.1, 0.1, 0.1, 0.1, "cm")
    ) +
    labs(
        x = "Average Excess Official CPI (%)",
        y = "Average Excess WS-CPI (%)"
    ) +
    coord_cartesian() +
    scale_x_continuous(
        expand = expansion(mult = 0.5)
    )


# Display the scatterplot
scatterplot
# With label >> names
# ggsave("excess_differences.eps", plot = scatterplot, path="images", width=11, height=9)
# with label >> code
ggsave("excess_differences_code.eps", plot = scatterplot, path="images", width=11, height=9)

rank_corr
num_corr

# Disconnect from DB
dbDisconnect(conn = con)


