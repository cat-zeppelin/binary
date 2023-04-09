#' Log Loss
#' @export
log_loss <- function(p, y) {
    x <- -sum(y * log(p) + (1 - y) * log(1 - p)) / length(p)
    x
}


