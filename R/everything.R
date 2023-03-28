#' Moments
#' @export
moments <- function(x) {
    n <- length(x)

    m1 <- mean(x)

    d <- x - m1
    m2 <- d * d
    m3 <- m2 * d
    m4 <- m3 * d

    m2 <- sum(m2) / (n - 1)
    m3 <- sum(m3) / n
    m4 <- sum(m4) / n

    sd <- sqrt(m2)

    list(
        mean = m1,
        var = m2,
        sd = sd,
        rsd = sd / m1,
        skewness = m3 / m2^(3/2),
        kurtosis = m4 / m2^2 - 3
    )
}


#' Probability to Score
#' @export
p2s <- function(p, score = 660, odds = 72, pdo = 40) {
    o <- (1 - p) / p
    s <- score + pdo * log(o / odds, 2)
    s
}


#' Score to Probability
#' @export
s2p <- function(s, score = 660, odds = 72, pdo = 40) {
    o <- (s - score) / pdo
    o <- odds * 2^o
    p <- 1 / (1 + o)
    p
}
