#' Given a call, create a tidy string
#' @param call A call
#' @return A string
#' @importFrom rlang expr_text
#' @importFrom stringr str_squish str_replace_all
#' @keywords internal

call_to_string <- function(call) {
  x <- rlang::expr_text(call)
  x <- stringr::str_squish(x)
  x <- gsub("\\), ", "\\),\n", x)
  x
}

#' Take a list of calls, return a string
#' Each element of the list will be converted to a string, and then
#' combined with the other elements
#' @param list_of_calls a list of calls
#' @param sep a character string to separate the items of `list_of_calls` when
#' they are converted to a single string
#' @return A length-one character vector; each element of `list_of_calls` will
#' be converted to a string; each string will be combined, separated by
#' `sep`
#' @keywords internal
calls_to_string <- function(list_of_calls, sep = " + \n") {
  x <- purrr::map_chr(list_of_calls,
                 call_to_string)

  x <- paste0(x, collapse = sep)
  x
}
