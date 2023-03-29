#' Palette
#' @export
default_palette <- function(i = NULL) {
    ggplot_palette(i)
}


#' Palette
#' @export
ggplot_palette <- function(i = NULL) {
    colors <- c(
        "#55bcc2",
        "#e77e72",
        "#88ac33",
        "#bd82f8",
        "#bebebe"
    )

    if (is_null(i)) {
        return(colors)
    }

    colors[i]
}
