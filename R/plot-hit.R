#' Positive Rate vs Score
#' @export
plot_hit <- function(s, y, n = 5, palette = default_palette()) {
    wt <- doply_binning(s, method = "equal", n = n) |>
        woe_table(y)

    plot <- ggplot(wt, aes(x = x, y = PositiveRate)) +
        geom_col(
            aes(color = "Color", fill = "Fill"),
            linewidth = 0.5,
            alpha = 0.2
        ) +
        geom_label(
            aes(label = glue("{round(100 * PositiveRate, 1)}%")),
            vjust = -0.3
        ) +
        labs(
            title = "Positive Rate vs Score",
            subtitle = glue("Inf. Value = {round(sum(wt$IV), 2)}"),
            x = "Bin (eq. sized)",
            y = "Positive Rate"
        ) +
        ylim(0, max(wt$PositiveRate) * 1.05) +
        theme_light() +
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold")
        ) +
        scale_fill_manual(
            values = c("Fill" = palette[1])
        ) +
        scale_color_manual(
            values = c("Color" = palette[1])
        )

    plot
}
