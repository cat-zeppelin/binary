#' Binary
#' @export
TPN <- function(x, y, w = NULL) {
    w <- if_null(w, rep(1, length(x)))
    tpn <- tpn_dplyr(x, y, w)
    tpn
}

#' Binary
#' @export
tpn_dplyr <- function(x, y, w) {
    df <- tibble(x, y, w) |>
        group_by(x) |>
        summarise(
            TotalCount = sum(w),
            PositiveCount = sum(w * y),
            NegativeCount = TotalCount - PositiveCount
        )
    df
}
