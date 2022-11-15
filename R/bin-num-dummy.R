
#' @export
bin_num_dummy <- function(x, y, w, p) {

    i <- !is_na(x)
    x <- x[i]
    y <- y[i]
    w <- w[i]

    q <- p * length(i) / sum(i)

    tpn <- TPN(x, y, w) |> rename(t = t_cnt, p = p_cnt, n = n_cnt)
    tpn <- tpn |>
        mutate_at(
            c("t", "p", "n"),
            function(col) cumsum(col) / sum(col)
        ) |>
        mutate(
            iv = (n - p) * log(n / p) + ((1 - n) - (1 - p)) * log((1 - n) / (1 - p)),
            iv = if_else(t >= q & t <= (1 - q), iv, 0)
        )

    cp <- tpn$x[which_max(tpn$iv)]
    bin <- list(cut_points = cp, na_bin = NA)
    bin
}


