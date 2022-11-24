#' Binary
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @export
confusion_table <- function(x, y, w = NULL, desc = NULL) {
    cm <- TPN(x, y, w)

    if (desc) {
        cm <- cm |> arrange(desc(x))
    }

    T_TOTAL <- sum(cm$t_cnt)
    P_TOTAL <- sum(cm$p_cnt)
    N_TOTAL <- sum(cm$n_cnt)

    cm <- cm |>
        mutate(
            T_CNT = cumsum(t_cnt),
            P_CNT = cumsum(p_cnt),
            N_CNT = cumsum(n_cnt),

            t_cnt = NULL,
            p_cnt = NULL,
            n_cnt = NULL,

            T_CDF = T_CNT / T_TOTAL,
            P_CDF = P_CNT / P_TOTAL,
            N_CDF = N_CNT / N_TOTAL,

            TP = P_CNT,
            TN = N_TOTAL - N_CNT,
            FP = N_CNT,
            FN = P_TOTAL - P_CNT,

            TPR = TP / P_TOTAL,
            TNR = TN / N_TOTAL,
            FPR = FP / N_TOTAL,
            FNR = FN / P_TOTAL,

            PPV = TP / (TP + FP),
            NPV = TN / (TN + FN),
            FDR = FP / (FP + TP),
            FOR = FN / (FN + TN),

            ACC = (TP + TN) / T_TOTAL,
            BA = (TPR + TNR) / 2,

            F1 = 2 * PPV * TPR / (PPV + TPR),

            N = T_TOTAL,
            S = (TP + FN) / N,
            P = (TP + FP) / N,

            MCC = (TP / N - S * P) / sqrt(P * S * (1 - S) * (1 - P)),

            N = NULL,
            S = NULL,
            P = NULL
        )

    class(cm) <- c("confusion_table", class(cm))
    cm
}

library(dplyr)
