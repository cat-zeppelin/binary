
value_encoder <- function(x, y = NULL, w = NULL, wt = NULL, value = "woe", f = NULL) {

    # preconditions ----

    if (!is_character(x)) {
        stop("`x` must be a character vector")
    }

    # ...

    if (!is_null(wt)) {
        if (!inherits(wt, "woe_table")) {
            stop("`wt` must be a `woe_table`")
        }

        if (!is_subset(x, wt$x)) {
            stop("`wt` does not contain all of `x` values")
        }

        if (!is_element(x, names(wt))) {
            stop("`value` must be one of `wt` columns")
        }
    }


    # body ----


}
