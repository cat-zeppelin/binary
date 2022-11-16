#' @export
value_encoder <- function(x, y = NULL, w = NULL, wt = NULL, value = "woe", f = NULL) {

    # preconditions ----

    if (!is_character(x)) {
        stop("`x` must be a character vector")
    }

    # ...

    if (is_null(wt)) {
        # TODO: add traditional checks for `y`
        wt <- woe_table(x, y, w)
    }

    # ...

    if (!inherits(wt, "woe_table")) {
        stop("`wt` must be a `woe_table`")
    }

    if (!is_subset(x, wt$x)) {
        stop("`wt` does not contain all of `x` values")
    }

    if (!is_element(x, names(wt))) {
        stop("`value` must be one of `wt` columns")
    }

    # body ----

    if (!is_null(f)) {
        values <- f(wt)
    } else {
        values <- pluck(wt, value)
    }

    names(values) <- wt$x
    x <- values[x]
    x
}
