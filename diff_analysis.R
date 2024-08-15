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

diff_df$cpi_diff <- rowMeans(subset(diff_df, select=-c(date)))

off_df$cpi_diff <- rowMeans(subset(off_df, select=-c(date)))

# Average value of excess CPI
diff_post <- diff_df %>% filter(date > as.Date("2022-02-24")) %>%
    select(cpi_diff) %>% colMeans()

# Max and min value for graph
diff_df$max <- apply(subset(diff_df, select = -c(date)), 1, max, na.rm=TRUE)
diff_df$min <- apply(subset(diff_df, select = -c(date)), 1, min, na.rm=TRUE)

# Average value of excess CPI - official data
off_post <- off_df %>% filter(date > as.Date("2022-02-24")) %>%
    select(cpi_diff) %>% colMeans()

# Max and min value for graph - official data
off_df$max <- apply(subset(off_df, select = -c(date)), 1, max, na.rm=TRUE)
off_df$min <- apply(subset(off_df, select = -c(date)), 1, min, na.rm=TRUE)



graph_df <- diff_df %>% select(c(date, cpi_diff, max, min)) %>%
    filter(date > as.Date("2022-02-24"))

graph_off_df <- off_df %>% select(c(date, cpi_diff, max, min)) %>%
    filter(date > as.Date("2022-02-24"))

graph_df <- full_join(graph_df, graph_off_df, by= "date", suffix = c("", "_off")) %>%
    mutate(cpi_diff_off_all = round(na_interpolation(cpi_diff_off, option="linear"), n_dec)) %>%
    mutate(min_off_all = round(na_interpolation(min_off, option="linear"), n_dec)) %>%
    mutate(max_off_all = round(na_interpolation(max_off, option="linear"), n_dec))

g_diff <- ggplot(data = graph_df, aes(x=date)) +
    geom_line(aes(y=cpi_diff, colour="Web Scraping Average")) +
    geom_line(aes(y=min, colour="Web Scraping Min/Max"), linetype="dashed", na.rm=TRUE) +
    geom_line(aes(y=max, colour="Web Scraping Min/Max"), linetype="dashed", na.rm=TRUE) +
    geom_line(aes(y=cpi_diff_off_all, colour="Official Average")) +
    geom_point(aes(y=cpi_diff_off, colour="Official Average"), size=2, na.rm=TRUE) +
    geom_line(aes(y=min_off_all, colour="Official Min/Max"), linetype="dashed", na.rm=TRUE) +
    geom_point(aes(y=min_off, colour="Official Min/Max"), size=2, na.rm=TRUE) +
    geom_line(aes(y=max_off_all, colour="Official Min/Max"), linetype="dashed", na.rm=TRUE) +
    geom_point(aes(y=max_off, colour="Official Min/Max"), size=2, na.rm=TRUE) +
    labs(
        x = NULL,
        y = "Percentage points",
        colour = "Excess CPI level"
    ) +
    scale_x_date(expand = expansion(mult = 0), date_labels = "%m-%Y")  +
    theme_minimal() +
    theme(text=element_text(family="bookantiqua"))

excess_diff <- graph_df %>%
    mutate(difference = cpi_diff - cpi_diff_off) %>%
    select(c(difference)) %>%
    colMeans(na.rm=TRUE)

# Save EPS and PNG
ggsave("cpi_diff.eps", plot = g_diff, path="images", device="eps")
#ggsave("cpi_diff.png", plot = g_diff, path="images")
#ggsave("cpi_diff.pdf", plot = g_diff, path="images")
#ggsave("cpi_diff.tex", plot = g_diff, path="images")

# Disconnect from DB
dbDisconnect(conn = con)
