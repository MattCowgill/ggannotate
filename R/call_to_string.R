#' Given a call, create a tidy string
#' @param call A call
#' @return A string
#' @keywords internal

call_to_string <- function(call) {
  single <- paste(deparse(call, width.cutoff = 500L), collapse = "")
  if (nchar(single) <= 80L) {
    return(single)
  }
  format_call_multiline(call, indent = 0L)
}

format_call_multiline <- function(call, indent = 0L) {
  fn_name <- deparse(call[[1L]])
  args <- as.list(call[-1L])
  arg_names <- names(args)
  inner_indent <- indent + 2L
  inner_prefix <- strrep(" ", inner_indent)

  arg_strs <- character(length(args))
  for (i in seq_along(args)) {
    val <- args[[i]]
    val_str <- paste(deparse(val, width.cutoff = 500L), collapse = "")

    if (nchar(val_str) > 60L && is.call(val)) {
      val_str <- format_call_multiline(val, indent = inner_indent)
    }

    nm <- if (!is.null(arg_names) && nzchar(arg_names[i])) {
      paste0(arg_names[i], " = ")
    } else {
      ""
    }
    arg_strs[i] <- paste0(inner_prefix, nm, val_str)
  }

  paste0(
    fn_name,
    "(\n",
    paste(arg_strs, collapse = ",\n"),
    "\n",
    strrep(" ", indent),
    ")"
  )
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
calls_to_string <- function(list_of_calls, sep = " +\n") {
  x <- purrr::map_chr(
    list_of_calls,
    call_to_string
  )

  paste0(x, collapse = sep)
}
