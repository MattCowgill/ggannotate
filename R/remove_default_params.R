#' Given a list of parameters and aes for a ggplot2 geom,
#' remove elements of the list if they are identical to the geom defaults
#' @param geom_as_string ggplot2 geom as a string, as in "geom_text"
#' @param params list of parameters and/or aesthetics, as in
#' list(colour = "black", foo = "bar")
#'
#' @return A list of parameters + aesthetics in which none of the values
#' correspond to the geom defaults
#' @noRd
remove_default_params <- function(geom_as_string, params) {
  default_params <- get_geom_defaults(geom_as_string)

  # If we couldn't get defaults, return params unchanged
  if (length(default_params) == 0) {
    return(params)
  }

  param_names <- names(params)

  for (param in param_names) {
    # Note: using all.equal() rather than identical() because we want to
    # treat integers and doubles as being equal
    if (isTRUE(all.equal(params[[param]], default_params[[param]]))) {
      params[[param]] <- NULL
    }
  }

  params
}
