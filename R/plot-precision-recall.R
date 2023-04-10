#' Precision-Recall
#' @export
plot.PrecisionRecall <- function(pr, palette = default_palette()) {
    plot_precision_recall(pr, palette)
}


#' Precision-Recall
#' @export
plot_precision_recall <- function(pr, palette = default_palette()) {
    recall <- pr$recall
    precision <- pr$precision
    auc <- pr$AUC
    auc0 <- pr$AUC0
    mean_y <- auc0

    auc <- round(100 * auc, 1)
    auc0 <- round(100 * auc0, 1)

    i <- precision >= mean_y
    recall <- recall[i]
    precision <- precision[i]

    plot <- ggplot() +
        geom_line(
            mapping = aes(x = Recall, y = Precision, color = "PR"),
            data = tibble(Recall = recall, Precision = precision),
            linewidth = 1
        ) +
        geom_line(
            mapping = aes(x = recall, y = mean_y, color = "PR0"),
            data = tibble(recall, mean_y),
            linetype = "dashed"
        ) +
        geom_ribbon(
            mapping = aes(x = x, ymin = ymin, ymax = ymax, fill = "Fill"),
            data = tibble(
                x = recall,
                ymin = mean_y,
                ymax = precision
            ),
            alpha = 0.1
        ) +
        labs(
            title = "Precision-Recall Curve",
            subtitle = glue("AUC = {auc}, No Skill AUC = {auc0}")
        ) +
        theme_light() +
        theme(
            plot.title = element_text(face = "bold"),
            legend.position = "none"
        ) +
        scale_fill_manual(
            values = c("Fill" = palette[1])
        ) +
        scale_color_manual(
            values = c("PR" = palette[1], "PR0" = palette[5])
        )

    plot
}
