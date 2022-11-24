#' Binary
#' @export
kolmogorov_smirnov <- function(s, y, w = NULL) {
    cm <- confusion_table(s, y, w, desc = FALSE)

    score <- cm$x
    P_CDF <- cm$P_CDF
    N_CDF <- cm$N_CDF

    ks <- list(
        score = score,
        P_CDF = P_CDF,
        N_CDF = N_CDF,
        KS = max(abs(P_CDF - N_CDF)),
        at = score[which_max(abs(P_CDF - N_CDF))]
    )

    class(ks) <- c("KS", class(ks))
    ks
}
