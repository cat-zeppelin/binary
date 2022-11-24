#' Binary
#' @export
TPN <- function(x, y, w = NULL) {
    w <- if_null(w, rep(1, length(x)))
    df <- tpn_dplyr(x, y, w)
    df
}


#' @importFrom tibble tibble
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @export
tpn_dplyr <- function(x, y, w) {
    df <- tibble(x, y, w) |>
        group_by(x) |>
        summarise(
            t_cnt = sum(w),
            p_cnt = sum(w * y),
            n_cnt = t_cnt - p_cnt
        )
    df
}
