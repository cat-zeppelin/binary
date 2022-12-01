#' Binary
#' @import nodots
#' @export
do_binning <- function(x, y = NULL, w = NULL, method = NULL, ...) {
    if (is_numeric(x)) {
        bin <- do_bin_num(x, y, w, method, ...)
    } else {
        bin <- do_bin_cat(x, y, w, method, ...)
    }
    bin
}


#' Binary
#' @export
doply_binning <- function(x, y = NULL, w = NULL, method = NULL, ...) {
    bin <- do_binning(x, y, w, mehtod, ...)
    x <- apply_binning(x, bin)
    x
}



#' Binary
#' @export
apply_binning <- function(x, bin) {
    if (is_numeric(x)) {
        x <- apply_bin_num(x, bin)
    } else {
        x <- apply_bin_cat(x, bin)
    }
    x
}








