library(readr)
library(dplyr)
library(biglm)
library(DBI)
library(ggplot2)
library(showtext)
library(patchwork)

# Add mathpazo font
font_add(family = "mathpazo",
         regular = "/home/luigi/.local/share/fonts/fplmr.pfb",
         bold = "/home/luigi/.local/share/fonts/fplmb.pfb",
         italic = "/home/luigi/.local/share/fonts/fplmri.pfb",
         bolditalic = "/home/luigi/.local/share/fonts/fplmbi.pfb",
         symbol = "/home/luigi/.local/share/fonts/fplmbb.pfb")

# Add Book Antiqua font
font_add(family = "bookantiqua",
         regular = "/home/luigi/.local/share/fonts/book-antiqua.ttf")

load_showtext_fonts()
showtext_auto(enable = TRUE)


# TPD function
calculate_tpd <- function(data_df, flag) {
    # Switch for prices and stock
    if (flag == "cpi") {
        data_df$log_target <- log(data_df$price)
    } else {
        data_df$log_target <- log(data_df$stock)
    }
    # Calculate model
    model <- biglm(log_target ~ date_cat + name, data = data_df)
    coeff <- coef(model)
    # Drop the model to recover memory
    rm(model)
    gc()
    # Elaborate results
    result_df <- as.data.frame(coeff[grep("^date_cat", names(coeff))])
    result_df$date <- rownames(result_df)
    colnames(result_df) <- c("tpd", "date")
    result_df$date <- gsub("date_cat", "", as.character(result_df$date))
    result_df$tpd <- (exp(result_df$tpd) * 100)
    # Add base value for Feb 28th 2021
    result_df <- add_row(result_df,
                         tpd = 100,
                         date = "2021-02-28",
                         .before = 2)
    # Reset rownames
    rownames(result_df) <- NULL
    return(result_df)
}


results_path <- file.path(getwd(), "elaborations")

con <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(results_path, paste0("champagne_did.db")))


# Data on wine, champage and prosecco
all_data_df <- read_csv("elaborations/champagne_did_data.csv",
                        col_types = cols(date = col_date(format = "%Y-%m-%d")))

# Only select Champagne and Prosecco
data_df <- all_data_df %>%
    filter(champagne==TRUE|prosecco==TRUE)

# Data as factors
data_df$name <- as.factor(data_df$name)
data_df$date_cat <- as.factor(data_df$date)
data_df$date_cat <- relevel(data_df$date_cat, ref = "2021-02-28")
data_df <- as.data.frame(data_df)

# Champagne and Prosecco split
champagne_df <- data_df %>%
    filter(champagne==TRUE)

prosecco_df <- data_df %>%
    filter(prosecco==TRUE)

# CPI and PSI calculation
champagne_cpi <- calculate_tpd(champagne_df, "cpi") %>%
    rename("champagne_cpi" = "tpd") %>%
    mutate(date = as.Date(date))
prosecco_cpi <- calculate_tpd(prosecco_df, "cpi") %>%
    rename("prosecco_cpi" = "tpd")  %>%
    mutate(date = as.Date(date))

champagne_psi <- calculate_tpd(champagne_df, "stock") %>%
    rename("champagne_psi" = "tpd") %>%
    mutate(date = as.Date(date))
prosecco_psi <- calculate_tpd(prosecco_df, "stock") %>%
    rename("prosecco_psi" = "tpd") %>%
    mutate(date = as.Date(date))

# Average prices calculation
champagne_avg <- champagne_df %>%
    group_by(date) %>%
    mutate(champagne_avg = mean(price)) %>%
    mutate(champagne_n = n()) %>%
    mutate(champagne_brands = n_distinct(brand)) %>%
    ungroup() %>%
    select(date, champagne_avg, champagne_n, champagne_brands) %>%
    distinct() %>%
    arrange(date)

prosecco_avg <- prosecco_df %>%
    group_by(date) %>%
    mutate(prosecco_avg = mean(price)) %>%
    mutate(prosecco_n = n()) %>%
    mutate(prosecco_brands = n_distinct(brand)) %>%
    ungroup() %>%
    select(date, prosecco_avg, prosecco_n, prosecco_brands) %>%
    distinct() %>%
    arrange(date)

index_df <- full_join(champagne_cpi, prosecco_cpi, by="date") %>%
    full_join(., champagne_psi, by="date") %>%
    full_join(., prosecco_psi, by="date") %>%
    full_join(., champagne_avg, by="date") %>%
    full_join(., prosecco_avg, by="date")



#### CPI PLOT ####
cpi_plot <- ggplot(data = index_df, aes(x=date)) +
    geom_line(aes(y=champagne_cpi, colour="Champagne")) +
    geom_line(aes(y=prosecco_cpi, colour="Prosecco")) +
    labs(
        x = NULL,
        y = "CPI",
        colour = "Product",
        subtitle = "Consumer Price Index"
    ) +
    scale_x_date(expand = expansion(mult = 0)) +
    geom_vline(xintercept=as.Date("2022-02-24"), linetype="dashed", color = "black", linewidth=0.3) +
    annotate("text", x = as.Date("2022-02-20"),
             y = max(c(max(index_df$champagne_cpi, na.rm=TRUE), max(index_df$prosecco_cpi, na.rm=TRUE))),
             label = "2022-02-24\nRussia attacks Ukraine",
             size=2, hjust="right", vjust="inward", family = "bookantiqua") +
    geom_vline(xintercept=as.Date("2022-03-15"), linetype="dashed", color = "black", linewidth=0.3) +
    annotate("text", x = as.Date("2022-03-20"),
             y = 110,
             label = "2022-03-15\nBan on Champagne\nexport to Russia",
             size=2, hjust="left", vjust="inward", family = "bookantiqua")

price_plot <- ggplot(data = index_df, aes(x=date)) +
    geom_line(aes(y=champagne_avg, colour="Champagne")) +
    geom_line(aes(y=prosecco_avg, colour="Prosecco")) +
    labs(
        x = NULL,
        y = "Rubles",
        colour = "Product",
        subtitle = "Average Price"
    ) +
    scale_x_date(expand = expansion(mult = 0)) +
    geom_vline(xintercept=as.Date("2022-02-24"), linetype="dashed", color = "black", linewidth=0.3) +
    geom_vline(xintercept=as.Date("2022-03-15"), linetype="dashed", color = "black", linewidth=0.3)

p_plot <- cpi_plot / price_plot +
    plot_layout(ncol = 1, guides = "collect") & theme_minimal()
p_plot <- p_plot & theme(text=element_text(family="bookantiqua"))
# Save EPS
ggsave("champagne_cpi.eps", plot = p_plot, path="images")


#### PSI PLOT ####
psi_plot <- ggplot(data = index_df, aes(x=date)) +
    geom_line(aes(y=champagne_psi, colour="Champagne")) +
    geom_line(aes(y=prosecco_psi, colour="Prosecco")) +
    labs(
        x = NULL,
        y = "PSI",
        colour = "Product",
        subtitle = "Product Stock Index"
    ) +
    scale_x_date(expand = expansion(mult = 0)) +
    geom_vline(xintercept=as.Date("2022-02-24"), linetype="dashed", color = "black", linewidth=0.3) +
    annotate("text", x = as.Date("2022-02-20"),
             y = max(c(max(index_df$champagne_cpi, na.rm=TRUE), max(index_df$prosecco_cpi, na.rm=TRUE))),
             label = "2022-02-24\nRussia attacks Ukraine",
             size=2, hjust="right", vjust="inward", family = "bookantiqua") +
    geom_vline(xintercept=as.Date("2022-03-15"), linetype="dashed", color = "black", linewidth=0.3) +
    annotate("text", x = as.Date("2022-03-20"),
             y = 110,
             label = "2022-03-15\nBan on Champagne\nexport to Russia",
             size=2, hjust="left", vjust="inward", family = "bookantiqua")

sku_plot <- ggplot(data = index_df, aes(x=date)) +
    geom_line(aes(y=champagne_n, colour="Champagne")) +
    geom_line(aes(y=prosecco_n, colour="Prosecco")) +
    labs(
        x = NULL,
        y = NULL,
        colour = "Product",
        subtitle = "Number of products"
    ) +
    scale_x_date(expand = expansion(mult = 0)) +
    geom_vline(xintercept=as.Date("2022-02-24"), linetype="dashed", color = "black", linewidth=0.3) +
    geom_vline(xintercept=as.Date("2022-03-15"), linetype="dashed", color = "black", linewidth=0.3)

brand_plot <- ggplot(data = index_df, aes(x=date)) +
    geom_line(aes(y=champagne_brands, colour="Champagne")) +
    geom_line(aes(y=prosecco_brands, colour="Prosecco")) +
    labs(
        x = NULL,
        y = NULL,
        colour = "Product",
        subtitle = "Number of brands"
    ) +
    scale_x_date(expand = expansion(mult = 0)) +
    geom_vline(xintercept=as.Date("2022-02-24"), linetype="dashed", color = "black", linewidth=0.3) +
    geom_vline(xintercept=as.Date("2022-03-15"), linetype="dashed", color = "black", linewidth=0.3)

q_plot <- psi_plot / (sku_plot / brand_plot) +
    plot_layout(ncol = 1, guides = "collect") & theme_minimal()
q_plot <- q_plot & theme(text=element_text(family="bookantiqua"))

# Save EPS
ggsave("champagne_psi.eps", plot = q_plot, path="images")

# Combine images

combo_plot <- (cpi_plot / price_plot | psi_plot / (sku_plot / brand_plot)) / guide_area() +
    plot_layout(axes = "collect", guides = 'collect', heights = c(7,1)) & theme_minimal()

combo_plot <- combo_plot & theme(text=element_text(family="bookantiqua"), legend.position = 'bottom')

combo_plot

# Save EPS
ggsave("champagne_combo.eps", plot = combo_plot, path="images", width = 7,
       height = 7,
       units = "in")

#### DIFF IN DIFF ####
data_df <- data_df %>%
    mutate(period=as.numeric(as.factor(date))) %>%
    mutate(cohort=ifelse(champagne==TRUE, 51, 0)) %>%
    mutate(idname=as.numeric(name)) %>%
    mutate(champagne=as.numeric(champagne))


did_result <- did::att_gt(yname="price",
                    tname="period",
                    idname="idname",
                    gname="cohort",
                    data=data_df,
                    control_group= "nevertreated",
                    allow_unbalanced_panel = TRUE
                    )
#### DID PLOT ####
champagne_sanctions <- as.Date("2022-03-15")

did_plot_df <- tibble(did_result$att)
colnames(did_plot_df) <- "ate"
did_plot_df$se <- did_result$se
did_plot_df$date <- index_df$date[2:length(index_df$date)]
did_plot_df$post <- ifelse(did_plot_df$date>champagne_sanctions, TRUE,FALSE)



did_plot <- ggplot(did_plot_df, aes(x=date, y=ate, ymin = ate - did_result$c * se, ymax = ate + did_result$c * se)) +
    geom_point(aes(colour=post), size=1.5, show.legend = FALSE) +
    geom_errorbar(aes(colour=post), width=0.1, show.legend = FALSE) +
    scale_color_manual(values = c("blue", "red")) +
    geom_vline(xintercept=as.Date("2022-02-24"), linetype="dashed", color = "black", linewidth=0.3) +
    annotate("text", x = as.Date("2022-02-20"),
             y = 5000,
             label = "2022-02-24\nRussia attacks Ukraine",
             size=2.5, hjust="right", vjust="inward", family = "bookantiqua") +
    geom_vline(xintercept=as.Date("2022-03-15"), linetype="dashed", color = "black", linewidth=0.3) +
    geom_hline(yintercept = 0, color="black", linewidth=0.3)+
    annotate("text", x = as.Date("2022-03-20"),
             y = -2000,
             label = "2022-03-15\nBan on Champagne\nexport to Russia",
             size=2.5, hjust="left", vjust="inward", family = "bookantiqua") +
    labs(
        x = NULL,
        y = "ATT"
    )



did_plot <- did_plot & theme_minimal() & theme(text=element_text(family="bookantiqua"))

ggsave("champagne_did.eps", plot = did_plot, path="images")

did_plot
p_plot
q_plot

