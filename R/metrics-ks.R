#' Kolmogorov-Smirnov
#' @export
kolmogorov_smirnov <- function(s, y, w = NULL) {
    cm <- confusion_matrix(s, y, w, desc = FALSE)

    score <- cm$Cutoff
    positive_cdf <- cm$PositiveShare
    negative_cdf <- cm$NegativeShare
    diff <- abs(positive_cdf - negative_cdf)

    ks <- list(
        score = score,
        positive_cdf = positive_cdf,
        negative_cdf = negative_cdf,
        KS = max(diff),
        at = score[which_max(diff)]
    )

    class(ks) <- c("KS", class(ks))
    ks
}
