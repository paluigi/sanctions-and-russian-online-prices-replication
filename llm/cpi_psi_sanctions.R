library(dplyr)
library(tidyr)
library(DBI)
library(biglm)
library(ggplot2)
library(showtext)


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


tisod <- function(data_df, flag) {
    if (flag == "cpi") {
        data_df$log_target <- log(data_df$price)
    } else {
        data_df$log_target <- log(data_df$stock)
    }
    # Remove infinites and NAs. Infinites are bound to zero
    data_df[sapply(data_df, is.infinite)] <- 0
    data_df <- data_df[complete.cases(data_df), ]
    data_df <- data_df |>
        select(date_cat, sanctions, origin, log_target, name)
    # Calculate model
    #model <- biglm(log_target ~ date_cat:origin:sanctions + name, data = data_df)
    # Removed name because of collinearity with origin and sanctions
    model <- biglm(log_target ~ date_cat:origin:sanctions, data = data_df)
    coeff <- coef(model)
    # Elaborate results
    result_df <- as.data.frame(coeff[grep("^date_cat", names(coeff))])
    result_df$date <- rownames(result_df)
    colnames(result_df) <- c("index", "composite")
    result_df$composite <- gsub("date_cat", "", as.character(result_df$composite))
    result_df$index <- (exp(result_df$index) * 100)
    result_df <- result_df%>%
        separate(composite, into = c("date", "origin", "sanctions"), sep = ":", remove = TRUE) %>%
        mutate(origin= gsub("origin", "", as.character(origin))) %>%
        mutate(sanctions= gsub("sanctions", "", as.character(sanctions))) %>%
        mutate(sanctions=ifelse(sanctions==1, TRUE, FALSE)) %>%
        drop_na()
    return(result_df)
}

working_path <- file.path(getwd(), "llm")

categories <- c("5-3-1_majorappliances", "5-5-2_smalltools", "5-5-1_majortools", "5-3-2_smallappliances")
con <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(working_path, "product_classification.db"))

con_cpi <- dbConnect(RSQLite::SQLite(),
                 dbname = file.path(working_path, "cpi.db"))

con_psi <- dbConnect(RSQLite::SQLite(),
                     dbname = file.path(working_path, "psi.db"))

# Loop to calculate and save data
for (cat in categories) {
    for (measure in c("cpi", "psi")) {
        products <- dbReadTable(con, cat) |>
            mutate(name=factor(name)) |>
            mutate(origin=factor(origin)) |>
            mutate(sanctions=factor(sanctions)) |>
            mutate(date_cat=as.factor(date)) |>
            select(price, stock, name, date_cat, sanctions, origin)

        result_df <- tisod(products, measure)
        # Write to DB
        if (measure=="cpi") {
            dbWriteTable(con_cpi, cat, result_df, overwrite=TRUE)
        } else {
            dbWriteTable(con_psi, cat, result_df, overwrite=TRUE)
        }

        temp_plot <- ggplot(data=result_df |> mutate(date=as.Date(date))) +
            geom_line(aes(x=date, y=index, color=origin, linetype=sanctions)) +
            geom_vline(xintercept=as.Date("2022-02-24"), linetype="dashed", color = "black", linewidth=0.3) +
            annotate("text", x = as.Date("2022-02-20"),
                     y = max(result_df$index),
                     label = "2022-02-24\nRussia attacks Ukraine",
                     size=2.5, hjust="right", vjust="inward", family = "bookantiqua") +
            labs(
                y="Index",
                x="",
                color="Origin",
                linetype="Sanctions"
            ) +
            theme_light() +
            theme(legend.position = "bottom",
                  text=element_text(family="bookantiqua"))

        ggsave(filename = file.path(working_path, "images", paste0(cat, "_", measure, ".png")),
               plot=temp_plot,
               width = 15,
               height = 9,
               units = "cm",
               dpi = 300
               )
        ggsave(filename = file.path(working_path, "images", paste0(cat, "_", measure, ".eps")),
               plot=temp_plot,
               width = 15,
               height = 9,
               units = "cm",
               dpi = 300
        )

        prod_plot <- ggplot(data = products) +
            scale_alpha_discrete(range=c(0.5, 1)) +
            geom_bar(aes(x=as.Date(date_cat), fill=origin, alpha=sanctions)) +
            geom_vline(xintercept=as.Date("2022-02-24"), linetype="dashed", color = "black", linewidth=0.3) +
            labs(
                x="",
                y="N. Products",
                fill="Origin",
                alpha="Sanctions"
            ) +
            theme_light() +
            theme(legend.position = "bottom",
                  text=element_text(family="bookantiqua"))
        ggsave(filename = file.path(working_path, "images", paste0(cat, "_nprods_", measure, ".png")),
               plot=prod_plot,
               width = 15,
               height = 9,
               units = "cm",
               dpi = 300
        )
        ggsave(filename = file.path(working_path, "images", paste0(cat, "_nprods_", measure, ".eps")),
               plot=prod_plot,
               width = 15,
               height = 9,
               units = "cm",
               dpi = 300
        )

    }
}


# Disconnect from DB
dbDisconnect(conn = con)
dbDisconnect(conn = con_cpi)
dbDisconnect(conn = con_psi)

