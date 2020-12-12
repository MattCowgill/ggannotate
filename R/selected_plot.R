#' Parse text selected in the RStudio IDE as a ggplot2 plot
#' @param text String to parse as ggplot2 object;
#' default is `rstudio_selection()`
#' @return A ggplot2 plot
#' @keywords internal
selected_plot <- function(text = rstudio_selection()) {
  if (is.null(text)) {
    stop(
      "Please select some plot code before invoking ggannotate,",
      " or specify the name of the ggplot2 object you want to annotate."
    )
  }
  plot <- text_as_plot(text)

  if (!inherits(plot, "gg")) {
    stop("Could not parse text '",
         text,
         "' as a ggplot2 object.")
  }

  plot
}


# These functions help take a text string that a user has selected in RStudio
# and parse it, returning a ggplot2 object

# Some of these functions are adapted from the `reprex` package, written by:
# Jennifer Bryan; Jim Hester; David Robinson; and Hadley Wickham
# https://github.com/tidyverse/reprex

rstudio_selection <- function(context = rstudioapi::getSourceEditorContext()) {
  if (isFALSE(rstudioapi::isAvailable())) {
    stop(
      "ggannotate requires RStudio to see your selection.",
      " Supply `plot` instead."
    )
  }
  text <- rstudioapi::primary_selection(context)[["text"]]
  text
}

text_as_plot <- function(text) {
  x <- text
  x <- rstudio_text_tidy(x)
  x <- escape_newlines(sub("\n$", "", enc2utf8(x)))
  x <- paste(x, collapse = "")
  x <- rlang::parse_expr(x)
  eval(x)
}

rstudio_text_tidy <- function(x) {
  Encoding(x) <- "UTF-8"
  if (length(x) == 1) {
    ## rstudio_selection() returns catenated text
    x <- strsplit(x, "\n")[[1]]
  }

  n <- length(x)
  if (!grepl("\n$", x[[n]])) {
    x[[n]] <- newline(x[[n]])
  }
  x
}

newline <- function(x) {
  paste0(x, "\n")
}

escape_newlines <- function(x) {
  gsub("\n", "\\\\n", x, perl = TRUE)
}
