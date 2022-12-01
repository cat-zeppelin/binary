#' Binary
#' @export
plot_hit <- function(x, y, n = 5) {
    wt <- doply_binning(x, method = "equal", n = n) |>
        woe_table(y)

    plot <- ggplot(wt, aes(x = x, y = p_rate)) +
        geom_col(
            color = "#01bfc4",
            size = 0.5,
            fill = "#01bfc4",
            alpha = 0.2
        ) +
        geom_label(
            aes(label = round(p_rate, 2))
        ) +
        labs(
            title = "Positive Rate vs Score",
            subtitle = glue("Inf. Value = {round(sum(wt$iv), 2)}"),
            x = "Bin (eq. sized)",
            y = "Positive Rate"
        ) +
        theme_light() +
        theme(
            plot.title = element_text(face = "bold")
        )
    plot
}
