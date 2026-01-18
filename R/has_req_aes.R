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

  # Check each required aesthetic

  # ggplot2 uses "|" syntax for alternatives (e.g., "xend|yend" means xend OR yend)
  check_one_req <- function(req) {
    if (grepl("|", req, fixed = TRUE)) {
      # Split alternatives and check if ANY are present
      alternatives <- strsplit(req, "|", fixed = TRUE)[[1]]
      any(alternatives %in% actual_aes)
    } else {
      req %in% actual_aes
    }
  }

  all(vapply(req_aes, check_one_req, logical(1)))
}
