#' Given a call, create a tidy string
#' @param call A call
#' @importFrom rlang expr_text
#' @importFrom stringr str_squish str_replace_all

call_to_string <- function(call) {
  x <- rlang::expr_text(call)
  x <- stringr::str_squish(x)
  x <- stringr::str_replace_all(x, ", ", ",\n")
  x
}
