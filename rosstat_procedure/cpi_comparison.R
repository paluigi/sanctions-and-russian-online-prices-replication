library(DBI)
library(dplyr)
library(tidyr)
library(ggplot2)

rosstat_db <- file.path(getwd(), "rosstat_procedure", "r_cpi.db")
tpd_db <- file.path(getwd(), "elaborations", "cpi.db")
image_dir <- file.path(getwd(), "rosstat_procedure", "images" )

con_ross <- dbConnect(RSQLite::SQLite(),
                      dbname = rosstat_db)
con_tpd <- dbConnect(RSQLite::SQLite(),
                     dbname = tpd_db)

analytic_diffs <- data.frame()

for (table in dbListTables(con_ross)) {
    df <-  dbReadTable(con_tpd, table ) |>
        select(date, index, official) %>%
        merge(.,dbReadTable(con_ross, table), by="date", all=TRUE) |>
        mutate(date=as.Date(date))

    plot_df <- df |>
        rename("WS-CPI (TPD)" = "index", "RWS-CPI (Laspeyres)"="laspeyr", "Official"="official") |>
        pivot_longer(!date) |>
        drop_na()

    diff_df <- df |>
        mutate(ws_diff=((index-laspeyr)/index)*100) |>
        mutate(off_diff=((official-laspeyr)/index)*100) |>
        drop_na() |>
        summarise(ws_malpe=mean(ws_diff), ws_mape=mean(abs(ws_diff)),
                  official_malpe=mean(off_diff), official_mape=mean(abs(off_diff))) |>
        mutate(category=table)
    analytic_diffs <- bind_rows(analytic_diffs, diff_df)

    comp_plot <- ggplot(data=plot_df) +
        geom_line(aes(x=date, y=value, colour=name)) +
        labs(
            y="",
            x="",
            colour=""
        ) +
        theme_light() +
        theme(legend.position = "bottom")
    ggsave(filename = file.path(image_dir, paste0(table, ".png")), plot=comp_plot, width = 15, height = 9, units = "cm")
    ggsave(filename = file.path(image_dir, paste0(table, ".eps")), plot=comp_plot, width = 8, height = 6, units = "cm")

}
write.csv(analytic_diffs, file= file.path(getwd(), "rosstat_procedure", "cpi_diffs.csv"), row.names = FALSE)


# Disconnect from DB
dbDisconnect(conn = con_ross)
dbDisconnect(conn = con_tpd)
