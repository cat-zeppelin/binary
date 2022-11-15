
#' @export
bin_num_maxiv <- function(x, y, w, p, ...) {

    # preconditions ----

    n <- pluck(list(...), "n")

    if (is_null(n)) {
        stop("`bin_num_maxiv` requires `n` parameter")
    }

    ok <- is_numeric(n) && length(n) == 1 && is_finite(n) && between(n, 2, 10)

    if (!ok) {
        stop("`n` must be a number between 2 and 10")
    }

    # body ----

    i <- !is_na(x)
    x <- x[i]
    y <- y[i]
    w <- w[i]

    p <- p * length(i) / sum(i)

    bin <- bin_num_equal(x, w, method = "equal", n = 50)
    cp <- c(-Inf, bin$cut_points, Inf) |> unique() |> sort()
    x_cat <- cut(x, cp, labels = FALSE)
    wt <- woe_table(x_cat, y)

    # merge tiny bins ...

    while (min(wt$t_pdf) < p) {
        i <- which_min(wt$t_pdf)

        woe <- wt$woe
        woe_1 <- if_na(abs(woe[i] - woe[i + 1]), Inf)
        woe_2 <- ifelse(i == 1, Inf, abs(woe[i] - woe[i - 1]))

        if (woe_1 < woe_2) {
            i <- i + 1
        }

        cp <- cp[-i]
        x_cat <- cut(x, cp, labels = FALSE)
        wt <- woe_table(x_cat, y)
    }

    # reduce number of bins to `n` ...

    while (nrow(wt) > n) {
        woe_diff <- c(Inf, abs(diff(wt$woe)))
        i <- which_min(woe_diff)
        cp <- cp[-i]
        x_cat <- cut(x, cp, labels = FALSE)
        wt <- woe_table(x_cat, y)
    }

    bin <- list(cut_points = cp, na_bin = NA)
    bin
}
