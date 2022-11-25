#' Binary
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
plot_ks <- function(ks) {
    plot <- ggplot() +
        geom_line(
            mapping = aes(x = x, y = y, color = "Positive"),
            data = tibble(x = ks$score, y = ks$P_CDF),
            size = 1
        ) +
        geom_line(
            mapping = aes(x = x, y = y, color = "Negative"),
            data = tibble(x = ks$score, y = ks$N_CDF),
            size = 1
        ) +
        geom_vline(
            xintercept = ks$at,
            linetype = "dashed",
            color = "grey"
        ) +
        geom_ribbon(
            mapping = aes(x = x, ymin = ymin, ymax = ymax),
            data = tibble(
                x = ks$score,
                ymin = pmin(ks$N_CDF, ks$P_CDF),
                ymax = pmax(ks$N_CDF, ks$P_CDF)
            ),
            fill = "#01bfc4",
            alpha = 0.1
        ) +
        labs(
            title = glue("Score Cumulative Distribution"),
            subtitle = glue("K-S = {round(100 * ks$KS, 1)} @ {ks$at}"),
            x = "Score",
            y = "Distribution"
        ) +
        theme_light() +
        theme(
            legend.background = element_blank(),
            legend.position = c(0.15, 0.9),
            legend.title = element_blank(),
            plot.title = element_text(face = "bold")
        ) +
        scale_color_manual(
            breaks = c("Positive", "Negative"),
            values = c("Positive" = "#f8766d", "Negative" = "#01bfc4")
        )

    plot
}
