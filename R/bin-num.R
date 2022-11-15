
#' @export
do_num_binning <- function(x, y, w, method, p, ...) {

    # preconditions ----

    if (any(is_infinite(x))) {
        stop("`x` can not contain Inf values")
    }

    # ----

    bin <- switch(method,
        "equal" = bin_num_equal(x, w, ...),
        "freqs" = bin_num_freqs(x, w, ...),
        "dummy" = bin_num_dummy(x, y, w, p),
        "maxiv" = bin_num_maxiv(x, y, w, p, ...)
    )

    bin$cut_points <- c(-Inf, bin$cut_points, Inf) |> unique() |> sort()

    # TODO: implement normal NA handling
    bin$na_bin <- 0

    bin
}


#' @export
apply_num_binning <- function(x, bin) {

    # preconditions ----

    if (any(is_infinite(x))) {
        stop("`x` can not contain Inf values")
    }

    # ...

    if (any_na(x) && is_na(bin$na_bin)) {
        stop("`na_bin` is not specified while `x` contains NA")
    }

    # bin ----

    x_cat <- cut(x, bin$cut_points, labels = FALSE)
    x_cat <- if_na(x_cat, bin$na_bin)
    x_cat <- str_c(
        "bin_",
        str_pad(x_cat, nchar(max(x_cat)), "left", "0")
    )
    x_cat
}
