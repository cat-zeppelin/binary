#' Binary
#' @export
ROC <- function(p, y, w = NULL) {
    cm <- confusion_table(p, y, w, desc = TRUE)

    FPR <- c(0, cm$FPR)
    TPR <- c(0, cm$TPR)
    AUC <- AUC(FPR, TPR)

    roc <- list(
        FPR = FPR,
        TPR = TPR,
        AUC = AUC,
        Gini = 2 * AUC - 1
    )

    class(roc) <- c("ROC", class(roc))
    roc
}


#' @export
AUC <- function(x, y) {
    auc <- sum(diff(x) * (tail(y, -1) + head(y, -1)) / 2)
    auc
}
