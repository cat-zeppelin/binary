#' Matthews Correlation Coefficient
#' @export
plot_mcc <- function(cm, palette = default_palette()) {
    x <- cm$Cutoff
    mcc <- cm$MCC

    i <- !is_na(mcc)
    x <- x[i]
    mcc <- mcc[i]

    max_mcc <- max(mcc)
    max_mcc <- round(100 * max_mcc, 1)
    at <- x[which_max(mcc)]

    plot <- ggplot() +
        geom_line(
            mapping = aes(x = x, y = mcc, color = "Color"),
            data = tibble(x, mcc),
            linewidth = 1
        ) +
        geom_ribbon(
            mapping = aes(x = x, ymin = ymin, ymax = ymax, fill = "Fill"),
            data = tibble(
                x,
                ymin = 0,
                ymax = mcc
            ),
            alpha = 0.1
        ) +
        geom_vline(
            mapping = aes(xintercept = at, color = "At"),
            data = tibble(at),
            linetype = "dashed"
        ) +
        geom_point(
            mapping = aes(x = x, y = y, color = "Color"),
            data = tibble(x = at, y = max(mcc)),
            size = 2
        ) +
        labs(
            title = "Matthews Correlation Coefficient ",
            subtitle = glue("Max MCC = {max_mcc} at {at}")
        ) +
        xlab("Score") +
        ylab("MCC") +
        theme_light() +
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold")
        ) +
        scale_fill_manual(
            values = c("Fill" = palette[1])
        ) +
        scale_color_manual(
            values = c("Color" = palette[1], "At" = palette[5])
        )

    plot
}
