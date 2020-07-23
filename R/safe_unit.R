#' Functions to safely work with grid::unit() and grid::arrow() in Shiny
#' If an input is NULL, NULL is returned; otherwise return a call to
#' unit/arrow.
#'
#' This is needed because `unit(NULL, "inches")` gives an error,
#' whereas we want `NULL`.
#'
#' @param x Size of `grid::unit()` object
#' @param units Units of `grid::unit()` object
#' @param angle Angle of arrow created by `grid::arrow()`
#' @param length Length of arrow created by `grid::arrow()`
#' @param ends See `?grid::arrow()`
#' @param type See `?grid::arrow()`
#' @importFrom rlang call2

safe_unit <- function(x, units) {
  if (is.null(x) || is.null(units)) {
    NULL
  } else {
    rlang::call2("unit",
                 x, units)
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
      length <- safe_unit(length, "inches")

      arrow_params <- list(angle, length, ends, type)

      rlang::call2("arrow",
                   !!!arrow_params)
    }
  }
