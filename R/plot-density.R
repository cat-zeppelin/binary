#' Density
#' @export
plot_density <- function(s, y = NULL, palette = default_palette()) {
    if (is_null(y)) {
        plot <- plot_total_density(s, palette)
    } else {
        plot <- plot_pn_density(s, y, palette)
    }
    plot
}


plot_total_density <- function(s, palette) {
    m <- moments(s)

    plot <- ggplot() +
        geom_density(
            mapping = aes(x = s, color = "Color", fill = "Fill"),
            data = tibble(s),
            linewidth = 1,
            alpha = 0.1
        ) +
        geom_vline(
            mapping = aes(xintercept = m, color = "Color"),
            data = tibble(m = m$mean),
            linetype = "dashed"
        ) +
        labs(
            title = "Score Density",
            subtitle = glue("Mean = {round(m$mean, 1)}, SD = {round(m$sd, 1)}, Skewness = {round(m$skewness, 2)}"),
            x = "Score",
            y = "Density"
        ) +
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


plot_pn_density <- function(s, y, palette) {
    p_mean <- mean(s[y])
    n_mean <- mean(s[!y])

    plot <- ggplot() +
        geom_density(
            mapping = aes(x = s, color = "Positive", fill = "Positive"),
            data = tibble(s, y) |> filter(y),
            linewidth = 1,
            alpha = 0.1
        ) +
        geom_vline(
            mapping = aes(xintercept = p_mean, color = "Positive"),
            data = tibble(p_mean),
            linetype = "dashed"
        ) +
        geom_density(
            mapping = aes(x = s, color = "Negative", fill = "Negative"),
            data = tibble(s, y) |> filter(!y),
            linewidth = 1,
            alpha = 0.1
        ) +
        geom_vline(
            mapping = aes(xintercept = n_mean, color = "Negative"),
            data = tibble(n_mean),
            linetype = "dashed"
        ) +
        labs(
            title = "Score Density",
            subtitle = glue("Mean Positive = {round(p_mean)}, Negative = {round(n_mean)}"),
            x = "Score",
            y = "Density"
        ) +
        theme_light() +
        theme(
            legend.position = "none",
            plot.title = element_text(face = "bold")
        ) +
        scale_fill_manual(
            values = c(
                "Negative" = palette[1],
                "Positive" = palette[2]
            )
        ) +
        scale_color_manual(
            values = c(
                "Negative" = palette[1],
                "Positive" = palette[2]
            )
        )

    plot
}
