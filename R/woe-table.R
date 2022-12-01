#' Binary
#' @export
woe_table <- function(x, y, w = NULL) {
    wt <- TPN(x, y, w)
    wt <- wt |>
        mutate(
            t_pdf = t_cnt / sum(t_cnt),
            p_pdf = p_cnt / sum(p_cnt),
            n_pdf = n_cnt / sum(n_cnt),
            p_rate = p_cnt / t_cnt,
            woe = log(n_pdf / p_pdf),
            iv = woe * (n_pdf - p_pdf)
        )
    wt
}

