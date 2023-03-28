do_bin_num <- function(x, y, w, method, p, ...) {
    bin <- switch(
        method,
        "equal" = bin_num_equal(x, w, ...),
        "freqs" = bin_num_freqs(x, w, ...),
        "maxiv" = bin_num_maxiv(x, y, w, p, ...)
    )

    bin$cut_points <- c(-Inf, bin$cut_points, Inf) |>
        unique() |>
        sort()

    bin
}


apply_bin_num <- function(x, bin) {
    x <- cut(x, bin$cut_points, labels = FALSE)
    if (!is_null(bin$na_bin)) {
        x[is_na(x)] <- bin$na_bin
    }
    x
}
