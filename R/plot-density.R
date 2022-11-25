#' Binary
#' @export
plot_density <- function(x, y = NULL) {
    if (is_null(y)) {
        plot <- plot_total_density(x)
    } else {
        plot <- plot_pn_density(x, y)
    }
    plot
}


#' @export
plot_total_density <- function(x) {
    m <- moments(x)

    plot <- ggplot() +
        geom_density(
            mapping = aes(x = score),
            data = tibble(score = x),
            size = 1,
            color = "#01bfc4",
            fill = "#01bfc4",
            alpha = 0.1
        ) +
        geom_vline(
            xintercept = m$mean,
            linetype = "dashed",
            color = "#01bfc4"
        ) +
        labs(
            title = "Score Density",
            subtitle = glue("Mean = {round(m$mean, 1)}, SD = {round(m$sd, 1)}, Skewness = {round(m$skewness, 2)}"),
            x = "Score",
            y = "Density"
        ) +
        theme_light() +
        theme(
            plot.title = element_text(face = "bold")
        )

    plot
}


plot_pn_density <- function(x, y) {

    p_mean <- mean(s[y])
    n_mean <- mean(s[!y])

    plot <- ggplot() +
        geom_density(
            mapping = aes(x = score, color = "Positive"),
            data = tibble(score = s) |> filter(y),
            fill = "#f8766d",
            size = 1,
            alpha = 0.1
        ) +
        geom_vline(
            xintercept = p_mean,
            linetype = "dashed",
            color = "#f8766d"
        ) +
        geom_density(
            mapping = aes(x = score, color = "Negative"),
            data = tibble(score = s) |> filter(!y),
            fill = "#01bfc4",
            size = 1,
            alpha = 0.1
        ) +
        geom_vline(
            xintercept = n_mean,
            linetype = "dashed",
            color = "#01bfc4"
        ) +
        labs(
            title = "Score Density",
            subtitle = glue("Positive mean = {round(p_mean)}, Negative mean = {round(n_mean)}"),
            x = "Score",
            y = "Density"
        ) +
        theme_light() +
        theme(
            legend.background = element_blank(),
            legend.position = c(0.15, 0.85),
            legend.title = element_blank(),
            plot.title = element_text(face = "bold")
        ) +
        scale_color_manual(
            breaks = c("Positive", "Negative"),
            values = c("Positive" = "#f8766d", "Negative" = "#01bfc4")
        )

    plot
}
