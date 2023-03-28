#' Kolmogorov-Smirnov
#' @export
plot.KS <- function(ks) {
    plot_ks(ks)
}


#' Kolmogorov-Smirnov
#' @export
plot_ks <- function(ks) {
    s <- ks$score
    p_cdf <- ks$positive_cdf
    n_cdf <- ks$negative_cdf
    at <- ks$at
    ks <- ks$KS

    pp <- p_cdf[which(s == at)]
    pn <- n_cdf[which(s == at)]

    plot <- ggplot() +
        geom_line(
            mapping = aes(x = x, y = y, color = "Negative"),
            data = tibble(x = s, y = n_cdf),
            linewidth = 1
        ) +
        geom_line(
            mapping = aes(x = x, y = y, color = "Positive"),
            data = tibble(x = s, y = p_cdf),
            linewidth = 1
        ) +
        geom_vline(
            mapping = aes(xintercept = at, color = "At"),
            data = tibble(at = at),
            linetype = "dashed"
        ) +
        geom_point(
            mapping = aes(x = x, y = y, color = "Negative"),
            data = tibble(x = at, y = pn),
            size = 2
        ) +
        geom_point(
            mapping = aes(x = x, y = y, color = "Positive"),
            data = tibble(x = at, y = pp),
            size = 2
        ) +
        geom_ribbon(
            mapping = aes(x = x, ymin = ymin, ymax = ymax, fill = "Fill"),
            data = tibble(
                x = s,
                ymin = pmin(n_cdf, p_cdf),
                ymax = pmax(n_cdf, p_cdf)
            ),
            alpha = 0.1
        ) +
        labs(
            title = glue("Score Cumulative Distribution"),
            subtitle = glue("K-S = {round(100 * ks, 1)} at {round(at)}"),
            x = "Score",
            y = "Distribution"
        ) +
        theme_light() +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "none"
        ) +
        scale_fill_manual(
            values = c("Fill" = plot_palette(1))
        ) +
        scale_color_manual(
            values = c(
                "Negative" = plot_palette(1),
                "Positive" = plot_palette(2),
                "At" = plot_palette(5)
            )
        )

    plot
}
