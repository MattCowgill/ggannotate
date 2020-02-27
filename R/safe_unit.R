#' Functions to safely work with grid::unit() and grid::arrow() in Shiny
#' If an input is NULL, NULL is returned; otherwise return a unit/arrow object
#' This is needed because `unit(NULL, "inches")` gives an error,
#' whereas we want `NULL`.
#'
#' @param x Size of `grid::unit()` object
#' @param units Units of `grid::unit()` object
#' @param angle Angle of arrow created by `grid::arrow()`
#' @param length Length of arrow created by `grid::arrow()`
#' @param ends See `?grid::arrow()`
#' @param type See `?grid::arrow()`

safe_unit <- function(x, units) {
  if (is.null(x) || is.null(units)) {
    NULL
  } else {
    unit(x, units)
  }
}

#' @rdname safe_unit
safe_arrow <-
  function(angle,
           length,
           ends = "last",
           type = "closed") {
    if (is.null(angle) || is.null(length)) {
      NULL
    } else {
      arrow(
        angle = angle,
        length = safe_unit(length, "inches"),
        ends = ends,
        type = type
      )
    }
  }
