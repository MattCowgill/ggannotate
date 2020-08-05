#' Check if x and/or y-scales of a built ggplot2 object are date scales
#' @param built_plot A built ggplot2 object, created with `ggplot2::ggplot_build()`
check_if_date <- function(built_plot) {
  p <- built_plot

  x_scale <- p$layout$panel_scales_x[[1]]
  y_scale <- p$layout$panel_scales_y[[1]]

  x_date <- dplyr::if_else(
    inherits(x_scale, "ScaleContinuousDate"),
    TRUE,
    FALSE
  )

  y_date <- dplyr::if_else(
    inherits(y_scale, "ScaleContinuousDate"),
    TRUE,
    FALSE
  )

  list(
    "x_date" = x_date,
    "y_date" = y_date
  )
}

#' When you click on a plot that contains date scale(s), Shiny's
#' `input$plot_click` and similar lists return the clicked value as a number,
#' not a date
#' @noRd
num_to_date <- function(numdate) {
  if (is.null(numdate)) {
    NULL
  } else {
    as.Date(numdate, origin = "1970-01-01")
  }
}

#' The list object returned by Shiny (eg. `input$plot_click`) does not return
#' dates or flipped scales in a manner that can be easily dealt with. This
#' function corrects the date and/or flipped scales.
#' @param input_list List; returned by Shiny on input, such as `input$plot_click`
#' @param axis_classes List; output of `check_if_date()`
#' @param flipped_coords List; returned by `ggplot2::summarise_coord()`
#' @noRd

correct_scales <- function(input, axis_classes, flipped_coords) {
  if (isTRUE(flipped_coords)) {
    temp <- input

    input$y <- temp$x
    input$x <- temp$y
    input$xmin <- temp$ymin
    input$xmax <- temp$ymax
    input$ymin <- temp$xmin
    input$ymax <- temp$xmax
  }

  if (isTRUE(axis_classes$x_date)) {
    input$x <- num_to_date(input$x)
    input$xmin <- num_to_date(input$xmin)
    input$xmax <- num_to_date(input$xmax)
  }

  if (isTRUE(axis_classes$y_date)) {
    input$y <- num_to_date(input$y)
    input$ymin <- num_to_date(input$ymin)
    input$ymax <- num_to_date(input$ymax)
  }

  input
}
