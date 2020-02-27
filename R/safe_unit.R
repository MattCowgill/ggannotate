#' Functions to safely work with grid::unit() and grid::arrow() in Shiny
#' If an input is NULL, NULL is returned; otherwise return a unit/arrow object
#' @importFrom grid arrow unit
safe_unit <- function(x, units) {
  if (is.null(x)) {
    ret <- x
  } else {
    ret <- grid::unit(x, units)
  }
  return(ret)
}

safe_arrow <- function(angle, length, ends = "last", type = "closed") {
  if (is.null(angle) || is.null(length)) {
    ret <- NULL
  } else {
    ret <- grid::arrow(angle = angle,
                       length = safe_unit(length, "inches"),
                       ends = ends,
                       type = type)
  }
  return(ret)
}
