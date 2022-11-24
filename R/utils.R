#' Binary
#' @export
p2s <- function(p, score = 660, odds = 72, pdo = 40) {
    o <- (1 - p) / p
    s <- score + pdo * log(o / odds, 2)
    s
}


#' Binary
#' @export
s2p <- function(s, score = 660, odds = 72, pdo = 40) {
    o <- (s - score) / pdo
    o <- odds * 2^o
    p <- 1 / (1 + o)
    p
}
