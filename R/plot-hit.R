#' Positive Rate vs Score
#' @export
plot_hit <- function(s, y, n = 5) {
    wt <- doply_binning(s, method = "equal", n = n) |>
        woe_table(y)

    plot <- ggplot(wt, aes(x = x, y = p_rate)) +
        geom_col(
            aes(color = "Color", fill = "Fill"),
            linewidth = 0.5,
            alpha = 0.2
        ) +
        geom_label(
            aes(label = str_c(round(100 * p_rate, 1), "%")),
            vjust = -0.3
        ) +
        labs(
            title = "Positive Rate vs Score",
            subtitle = glue("Inf. Value = {round(sum(wt$iv), 2)}"),
            x = "Bin (eq. sized)",
            y = "Positive Rate"
        ) +
        ylim(0, max(wt$p_rate) * 1.05) +
        theme_light() +
        theme(
            plot.title = element_text(face = "bold")
        ) +
        scale_fill_manual(
            values = c("Fill" = plot_palette(1))
        ) +
        scale_color_manual(
            values = c("Color" = plot_palette(1))
        )

    plot
}
