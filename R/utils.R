# These helper functions are from the `reprex` package, written by:
# Jennifer Bryan; Jim Hester; David Robinson; and Hadley Wickham
# https://github.com/tidyverse/reprex

rstudio_selection <- function(context = rstudio_context()) {
  text <- rstudioapi::primary_selection(context)[["text"]]
}

rstudio_context <- function() {
  rstudioapi::getSourceEditorContext()
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

escape_newlines <- function (x) {
  gsub("\n", "\\\\n", x, perl = TRUE)
}

