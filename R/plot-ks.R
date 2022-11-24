#' Binary
#' @importFrom dplyr bind_rows
#' @importFrom tibble tibble
#' @export
plot_ks <- function(ks) {
    df <- bind_rows(
        tibble(label = "Negative CDF", x = ks$score, y = ks$N_CDF),
        tibble(label = "Positive CDF", x = ks$score, y = ks$P_CDF),
    )

    at <- ks$at
    ks <- round(100 * ks$KS, 1)

    plot <- ggplot() +
        geom_line(
            mapping = aes(x = x, y = y, color = label),
            data = df,
            size = 1
        ) +
        geom_line(
            mapping = aes(x = x, y = y),
            data = tibble(x = at, y = c(0, 1)),
            linetype = "dashed",
            color = "grey"
        ) +
        geom_ribbon(
            mapping = aes(x = x, ymin = N_CDF, ymax = P_CDF),
            data = df |> group_by(x) |> summarise(N_CDF = min(y), P_CDF = max(y)),
            fill = "#01bfc4",
            alpha = 0.1
        ) +
        labs(
            title = glue("Negative and Positive CDF, K-S = {ks} @ {at}"),
            x = "Score",
            y = "Probability, %"
        ) +
        theme_light() +
        theme(
            legend.background = element_rect(
                fill = "white",
                size = 0.3,
                linetype = "solid",
                color = "grey"
            ),
            legend.position = c(0.2, 0.85),
            legend.title = element_blank(),
            plot.title = element_text(face = "bold")
        )

    plot
}


#' @export
plot.KS <- function(ks) {
    plot_ks(ks)
}
