#' Check if x and/or y-scales of a built ggplot2 object are date or datetime scales
#' @param built_plot A built ggplot2 object, created with
#' `ggplot2::ggplot_build()`
#' @noRd
check_if_date <- function(built_plot) {
  x_scale <- get_panel_scale(built_plot, "x")
  y_scale <- get_panel_scale(built_plot, "y")

  list(
    "x_date" = is_date_scale(x_scale),
    "y_date" = is_date_scale(y_scale),
    "x_datetime" = is_datetime_scale(x_scale),
    "y_datetime" = is_datetime_scale(y_scale)
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

#' Convert numeric value to POSIXct datetime
#' Shiny returns datetime values as seconds since epoch
#' @noRd
num_to_datetime <- function(numdate) {
  if (is.null(numdate)) {
    NULL
  } else {
    as.POSIXct(numdate, origin = "1970-01-01", tz = "UTC")
  }
}

#' The list object returned by Shiny (eg. `input$plot_click`) does not return
#' dates or flipped scales in a manner that can be easily dealt with. This
#' function corrects the date and/or flipped scales.
#' @param input_list List; returned by Shiny on input,
#' such as `input$plot_click`
#' @param axis_classes List; output of `check_if_date()`
#' @param flipped_coords Logical; returned by `ggplot2::summarise_coord()$flip`
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

  # Handle Date scales
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

  # Handle datetime (POSIXct) scales
  if (isTRUE(axis_classes$x_datetime)) {
    input$x <- num_to_datetime(input$x)
    input$xmin <- num_to_datetime(input$xmin)
    input$xmax <- num_to_datetime(input$xmax)
  }

  if (isTRUE(axis_classes$y_datetime)) {
    input$y <- num_to_datetime(input$y)
    input$ymin <- num_to_datetime(input$ymin)
    input$ymax <- num_to_datetime(input$ymax)
  }

  input
}
