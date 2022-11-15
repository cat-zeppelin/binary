
#' @export
TPN <- function(x, y, w = NULL) {

    # preconditions ----

    ok <- is_numeric(x) || is_character(x)

    if (!ok) {
        stop("`x` must be either a numeric or a character vector")
    }

    # ...

    ok <- is_logical(y) && length(y) == length(x) && !any_na(y)

    if (!ok) {
        stop("`y` must be a logical vector, the same length as `x`, and without NA")
    }

    # ...

    w <- if_null(w, rep(1, length(x)))
    ok <- is_numeric(w) && length(w) == length(x) && all(is_finite(w))

    if (!ok) {
        stop("`w` must be a finite numeric vector of the same length as `x`")
    }

    # body ----

    tpn <- tpn_collapse(x, y, w)
    tpn
}


tpn_collapse <- function(x, y, w) {
    tpn <- tibble(x, t_cnt = w, p_cnt = w * y) |>
        fgroup_by(x) |>
        fsum() |>
        mutate(
            n_cnt = t_cnt - p_cnt
        )
    tpn
}
