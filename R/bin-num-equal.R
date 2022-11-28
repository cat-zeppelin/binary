bin_num_equal <- function(x, w, ...) {
    n <- pluck(list(...), "n")
    bin <- bin_num_freqs(x, w, q = rep(1, n))
    bin
}

