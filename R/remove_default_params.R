#' Given a list of parameters for a ggplot2 geom, remove elements of the list
#' if they are identical to the geom defaults
#' @noRd

remove_default_params <- function(geom_as_string, params) {

  get_param_defaults <- function(geom_as_string) {
    geom <- rlang::call2(geom_as_string)
    geom <- eval(geom)
    default_params <- geom$geom_params
    default_aes <- geom$geom$default_aes
    c(default_params, default_aes)
  }

  default_params <- get_param_defaults(geom_as_string)

  for (param in names(params)) {
    # Note: using all.equal() rather than identical() because we want to
    # treat integers and doubles as being equal
    if (isTRUE(all.equal(params[[param]], default_params[[param]]))) {
      params[[param]] <- NULL
    }
  }
  params
}
