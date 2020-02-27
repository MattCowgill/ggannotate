#' Check if x and/or y-scales of a built ggplot2 object are date scales
#' @param built_plot A built ggplot2 object, created with `ggplot2::ggplot_build()`
check_if_date <- function(built_plot) {
  p <- built_plot

  x_scale <- p$layout$panel_scales_x[[1]]
  y_scale <- p$layout$panel_scales_y[[1]]

  x_date <- dplyr::if_else(inherits(x_scale, "ScaleContinuousDate"),
                           TRUE,
                           FALSE)

  y_date <- dplyr::if_else(inherits(y_scale, "ScaleContinuousDate"),
                           TRUE,
                           FALSE)

  list("x_date" = x_date,
       "y_date" = y_date)
}
