#' WoE Table
#' @export
woe_table <- function(x, y, w = NULL) {
    wt <- TPN(x, y, w)
    wt <- wt |>
        mutate(
            TotalShare = TotalCount / sum(TotalCount),
            PositiveShare = PositiveCount / sum(PositiveCount),
            NegativeShare = NegativeCount / sum(NegativeCount),
            PositiveRate = PositiveCount / TotalCount,
            WoE = log(NegativeShare / PositiveShare),
            IV = WoE * (NegativeShare - PositiveShare)
        )
    class(wt) <- c("WoeTable", class(wt))
    wt
}
