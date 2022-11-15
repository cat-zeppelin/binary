
#' @export
bin_num_freqs <- function(x, w, ...) {

    # preconditions ----

    q <- pluck(list(...), "q")

    if (is_null(q)) {
        stop("`bin_num_freqs` requires `q` parameter")
    }

    ok <- is_numeric(q) && between(length(q), 2, 50) && all(is_finite(q))

    if (!ok) {
        stop("`q` must be a finite numeric vector with length between 2 and 50")
    }

    # body ----

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

    bin <- list(cut_points = x[i], na_bin = NA)
    bin
}
