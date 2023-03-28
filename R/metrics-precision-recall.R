#' Precision-Recall
#' @export
precision_recall <- function(p, y, w = NULL) {
    cm <- confusion_matrix(p, y, w, desc = TRUE)

    recall <- cm$TPR
    precision <- cm$PPV
    AUC <- AUC(recall, precision)

    pr <- list(
        recall = recall,
        precision = precision,
        AUC = AUC,
        AUC0 = mean(y)
    )

    class(pr) <- c("PrecisionRecall", class(pr))
    pr
}
