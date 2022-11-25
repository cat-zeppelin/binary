#' Binary
#' @import ggplot2
#' @importFrom glue glue
#' @export
plot_roc <- function(roc) {
    auc <- round(100 * roc$AUC, 1)
    gini <- round(100 * roc$Gini, 1)
    plot <- ggplot() +
        geom_line(
            mapping = aes(x = x, y = y),
            data = tibble(x = roc$FPR, y = roc$TPR),
            color = "#01bfc4",
            size = 1
        ) +
        geom_ribbon(
            mapping = aes(x = x, ymin = x, ymax = y),
            data = tibble(x = roc$FPR, y = roc$TPR),
            fill = "#01bfc4",
            alpha = 0.1
        ) +
        geom_line(
            mapping = aes(x = x, y = y),
            data = tibble(x = c(0, 1), y = c(0, 1)),
            linetype = "dashed",
            color = "grey"
        ) +
        labs(
            title = glue("ROC Curve"),
            subtitle = glue("AUC = {auc}, Gini = {gini}"),
            x = "False Positive Rate",
            y = "True Positive Rate"
        ) +
        theme_light() +
        theme(
            plot.title = element_text(face = "bold")
        )

    plot
}
