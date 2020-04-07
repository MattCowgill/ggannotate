# These functions help take a text string that a user has selected in RStudio
# and parse it, returning a ggplot2 object

# Some of these functions are adapted from the `reprex` package, written by:
# Jennifer Bryan; Jim Hester; David Robinson; and Hadley Wickham
# https://github.com/tidyverse/reprex

rstudio_selection <- function(context = rstudioapi::getSourceEditorContext()) {
  text <- rstudioapi::primary_selection(context)[["text"]]
  text
}

selection_as_plot <- function(selection) {
  x <- selection
  x <- rstudio_text_tidy(x)
  x <- escape_newlines(sub("\n$", "", enc2utf8(x)))
  x <- paste(x, collapse = "")
  x <- rlang::parse_expr(x)
  plot <- eval(x)
  plot
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
