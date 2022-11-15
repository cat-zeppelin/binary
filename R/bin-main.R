
#' @export
do_binning <- function(x, y = NULL, w = NULL, method = NULL, p = 0.1, ...) {

    # preconditions ----

    ok <- is_numeric(x) || is_character(x)

    if (!ok) {
        stop("`x` must be either a numeric or a character vector")
    }

    # ...

    BIN_METHODS <- c("dummy", "equal", "freqs", "maxiv")

    ok <- is_character(method) && length(method) == 1 && is_element(method, BIN_METHODS)

    if (!ok) {
        stop("`method` must be one of ", str_c("'", BIN_METHODS, "'", collapse = ", "))
    }

    # ...

    not_ok <- is_null(y) && !is_element(method, c("equal", "freqs"))

    if (not_ok) {
        stop("`y` must be specified if `method` is not 'equal' or 'freqs'")
    }

    # ...

    ok <- is_logical(y) && length(y) == length(x) && !any_na(y) ||
        is_element(method, c("equal", "freqs"))

    if (!ok) {
        stop("`y` must be a logical vector, the same length as `x`, and without NA")
    }

    # ...

    w <- if_null(w, rep(1, length(x)))
    ok <- is_numeric(w) && length(w) == length(x) && all(is_finite(w))

    if (!ok) {
        stop("`w` must be a finite numeric vector of the same length as `x`")
    }

    # ...

    ok <- is_numeric(p) && length(p) == 1 && is_finite(p) && between(p, 0.01, 0.3) ||
        is_element(method, c("equal", "freqs"))

    if (!ok) {
        stop("`p` must be a number between 0.01 and 0.3")
    }

    # body ----

    if (is_numeric(x)) {
        bin <- do_num_binning(x, y, w, method, p, ...)
    } else {
        bin <- do_cat_binning(x, y, w, method, p, ...)
    }

    bin
}


#' @export
apply_binning <- function(x, bin) {

    # preconditions ----

    ok <- is_numeric(x) || is_character(x)

    if (!ok) {
        stop("`x` must be either a numeric or a character vector")
    }

    # body ----

    if (is_numeric(x)) {
        x <- apply_num_binning(x, bin)
    } else {
        x <- apply_cat_binning(x, bin)
    }

    x
}


#' @export
doply_binning <- function(x, y = NULL, w = NULL, method = NULL, p = 0.1, ...) {
    bin <- do_binning(x, y, w, method, p, ...)
    x <- apply_binning(x, bin)
    x
}
