bin_num_freqs <- function(x, w, ...) {
    q <- pluck(list(...), "q")

    i <- !is_na(x)
    x <- x[i]
    w <- w[i]

    i <- order(x)
    x <- x[i]
    w <- w[i]

    w <- cumsum(w) / sum(w)
    q <- cumsum(q) / sum(q)
    q <- head(q, -1)
    i <- map_int(q, function(e) which_min(abs(w - e)))

    bin <- list(cut_points = x[i])
    bin
}
