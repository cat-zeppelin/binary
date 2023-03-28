#' Confusion Matrix
#' @export
confusion_matrix <- function(x, y, w = NULL, desc = NULL) {
    cm <- TPN(x, y, w)

    if (desc) {
        cm <- cm |> arrange(desc(x))
    }

    TOTAL <- sum(cm$TotalCount)
    POSITIVE <- sum(cm$PositiveCount)
    NEGATIVE <- sum(cm$NegativeCount)

    cm <- cm |>
        rename(Cutoff = x) |>
        mutate(
            TotalCount = cumsum(TotalCount),
            PositiveCount = cumsum(PositiveCount),
            NegativeCount = cumsum(NegativeCount),

            TotalShare = TotalCount / TOTAL,
            PositiveShare = PositiveCount / POSITIVE,
            NegativeShare = NegativeCount / NEGATIVE,

            TP = PositiveCount,
            TN = NEGATIVE - NegativeCount,
            FP = NegativeCount,
            FN = POSITIVE - PositiveCount,

            TPR = TP / POSITIVE,
            TNR = TN / NEGATIVE,
            FPR = FP / NEGATIVE,
            FNR = FN / POSITIVE,

            PPV = TP / (TP + FP),
            NPV = TN / (TN + FN),
            FDR = FP / (FP + TP),
            FOR = FN / (FN + TN),

            ACC = (TP + TN) / TOTAL,
            BA = (TPR + TNR) / 2,

            F1 = 2 * PPV * TPR / (PPV + TPR),

            N = TOTAL,
            S = (TP + FN) / N,
            P = (TP + FP) / N,

            MCC = (TP / N - S * P) / sqrt(P * S * (1 - S) * (1 - P)),

            N = NULL,
            S = NULL,
            P = NULL
        )

    class(cm) <- c("ConfusionMatrix", class(cm))
    cm
}
