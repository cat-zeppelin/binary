#' Binary
#' @import nodots
#' @export
do_binning <- function(x, y = NULL, w = NULL, method = NULL, ...) {
    if (is_numeric(x)) {
        bin <- do_bin_num(x, y, w, method, ...)
    } else {
        bin <- do_bin_cat(x, y, w, method, ...)
    }
    bin
}


do_bin_num <- function(x, y, w, method, ...) {
    bin <- switch(
        method,
        "equal" = bin_num_equal(x, w, ...),
        "freqs" = bin_num_freqs(x, w, ...),
        "dummy" = bin_num_dummy(x, y, w, ...),
        "maxiv" = bin_num_maxiv(x, y, w, ...)
    )

    bin$cut_points <- c(-Inf, bin$cut_points, Inf) |>
        unique() |>
        sort()

    bin
}


do_bin_cat <- function(x, y, w, method, ...) {
    NULL
}



