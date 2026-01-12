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
#' @noRd
has_req_aes <- function(geom) {
  req_aes <- get_required_aes(geom)
  actual_aes <- names(geom$mapping)

  # If we couldn't determine required aes, assume valid

  if (length(req_aes) == 0) {
    return(TRUE)
  }

  all(req_aes %in% actual_aes)
}
