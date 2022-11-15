
#' @export
bin_num_equal <- function(x, w, ...) {

    # preconditions ----

    n <- pluck(list(...), "n")

    if (is_null(n)) {
        stop("`bin_num_equal` requires `n` parameter")
    }

    ok <- is_numeric(n) && length(n) == 1 && is_finite(n) && between(n, 2, 50)

    if (!ok) {
        stop("`n` must be a number between 2 and 50")
    }

    # body ----

    bin <- bin_num_freqs(x, w, q = rep(1, n))
    bin
}
