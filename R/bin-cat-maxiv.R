

bin_cat_maxiv <- function(x, y, w, p, ...) {

    # filter out na ...

    i <- !is_na(x)
    x <- x[i]
    y <- y[i]
    w <- w[i]

    p <- p * length(i) / sum(i)

    # transform x to integer ...

    values_from <- sort(unique(x))
    values_to <- values_from
    x <- match(x, values_from)

    # merge
}
