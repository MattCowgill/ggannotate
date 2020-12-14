#' Check if a call to a ggplot2 geom has the required aes
#' @param geom A geom call, such as returned by `geom_text()`
#' @return Logical. `TRUE` if the call contains all required aesthetics.
#' @examples
#' library(ggplot2)
#' geom_a <- geom_text()
#' has_req_aes(geom_a) # returns `FALSE`
#'
#' geom_b <- geom_text(aes(x = 3, y = 30, label = "something"))
#' has_req_aes(geom_b) # returns `TRUE`
#' @keywords internal

has_req_aes <- function(geom) {
  req_aes <- geom$geom$required_aes
  actual_aes <- names(geom$mapping)
  all(req_aes %in% actual_aes)
}
