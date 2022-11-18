#' Binary
#' @description
#' Returns a pivot table with total, positive, and negative events count
#' @export
tpn_summary <- function(x, y, w = NULL) {
    w <- if_null(w, rep(1, length(x)))
    df <- tpn_dplyr(x, y, w)
    df
}


#' @importFrom tibble tibble
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
tpn_dplyr <- function(x, y, w) {
    df <- tibble(x, y, w) |>
        group_by(x) |>
        summarise(
            t = sum(w),
            p = sum(w * y),
            n = t - p
        )
    df
}
