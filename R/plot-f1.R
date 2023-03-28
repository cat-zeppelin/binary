#' F1 Score
#' @export
plot_f1 <- function(cm) {
    # model f1 score

    x <- cm$Cutoff
    f1 <- cm$F1

    i <- !is_na(f1)
    x <- x[i]
    f1 <- f1[i]

    max_f1 <- round(100 * max(f1), 1)
    at <- x[which_max(f1)]

    # no-skill f1 score
    #   precision is equal to an average sample positive rate since for a no-skill
    #   classifier positive rate for model positives / negatives must be equal
    #
    #   recall (TPR) of a no-skill classifier is equal to the
    #   total cumulative distribution

    t_cdf <- cm$TotalShare
    p_rate <- tail(cm$PPV, 1)

    f0 <- 2 * t_cdf * p_rate / (t_cdf + p_rate)
    f0 <- f0[i]

    plot <- ggplot() +
        geom_line(
            mapping = aes(x = x, y = f1, color = "F1"),
            data = tibble(x, f1),
            linewidth = 1
        ) +
        geom_line(
            mapping = aes(x = x, y = f0, color = "F0"),
            data = tibble(x, f0),
            linetype = "dashed"
        ) +
        geom_ribbon(
            mapping = aes(x = x, ymin = ymin, ymax = ymax, color = "Fill"),
            data = tibble(
                x,
                ymin = f0,
                ymax = f1
            ),
            alpha = 0.1
        ) +
        geom_vline(
            mapping = aes(xintercept = at, color = "At"),
            linetype = "dashed"
        ) +
        geom_point(
            mapping = aes(x = x, y = y, color = "F1"),
            data = tibble(x = at, y = max(f1)),
            size = 2
        ) +
        labs(
            title = "F1 Score",
            subtitle = glue("Max F1 Score = {max_f1} at {at}")
        ) +
        xlab("Score") +
        ylab("F1 Score") +
        theme_light() +
        theme(
            plot.title = element_text(face = "bold")
        ) +
        scale_fill_manual(
            values = c("Fill" = plot_palette(1))
        ) +
        scale_color_manual(
            values = c(
                "F1" = plot_palette(1),
                "F0" = plot_palette(5),
                "At" = plot_palette(5)
            )
        )

    plot
}
