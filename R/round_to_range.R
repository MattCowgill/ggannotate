round_to_range <- function(x, range) {
  if (!is.numeric(x)) {
    return(x)
  }
  r <- diff(range)
  if (r == 0) {
    return(x)
  }
  digits <- max(0, -floor(log10(r)) + 3)
  round(x, digits)
}
