#' Receiver Operating Characteristic
#' @export
plot.ROC <- function(roc) {
    plot_roc(roc)
}


#' Receiver Operating Characteristic
#' @export
plot_roc <- function(roc) {
    auc <- round(100 * roc$AUC, 1)
    gini <- round(100 * roc$Gini, 1)

    plot <- ggplot() +
        geom_line(
            mapping = aes(x = x, y = y, color = "ROC"),
            data = tibble(x = roc$FPR, y = roc$TPR),
            linewidth = 1
        ) +
        geom_line(
            mapping = aes(x = x, y = y, color = "ROC0"),
            data = tibble(x = c(0, 1), y = c(0, 1)),
            linetype = "dashed"
        ) +
        geom_ribbon(
            mapping = aes(x = x, ymin = x, ymax = y, fill = "Fill"),
            data = tibble(x = roc$FPR, y = roc$TPR),
            alpha = 0.1
        ) +
        labs(
            title = glue("ROC Curve"),
            subtitle = glue("ROC AUC = {auc}, Gini = {gini}"),
            x = "False Positive Rate",
            y = "True Positive Rate"
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
            values = c("ROC" = plot_palette(1), "ROC0" = plot_palette(5))
        )

    plot
}
