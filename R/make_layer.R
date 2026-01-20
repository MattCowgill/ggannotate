#' Create a ggplot2 annotation given some aesthetics, parameters, and
#' facet variables + values.
#'
#' This function is based on ggplot2::annotate(). The two main differences are:
#' it returns an unevaluated function call rather than a ggproto object; and
#' it can handle limiting annotations to (specified) facet level(s).
#'
#' @param geom Geom to annotate, such as "text".
#' @param aes Named list of aesthetics with corresponding data values, as in
#' `list(x = 3, y = 30, label = "A label")`.
#' @param params Optional. Named list of parameters for geom,
#' such as `list(colour = "black")`
#' @param facets Optional. Named list of facets; the name is the variable and
#' the value if the level, such as `list(cyl = 4)`.
#' @return A call
#'
#' @examples
#' library(ggplot2)
#'
#' base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point() +
#'   facet_wrap(~cyl)
#'
#' my_annot_call <- make_layer("text",
#'   aes = list(x = 3, y = 30, label = "A label"),
#'   facets = list(cyl = 6),
#'   params = list(col = "red")
#' )
#'
#' my_annotation <- eval(my_annot_call)
#'
#' base_plot + my_annotation
#' @export
#'
#' @importFrom purrr compact
#' @importFrom rlang call2 syms `!!!`
#' @importFrom stats setNames

make_layer <- function(geom,
                       aes = NULL,
                       params = NULL,
                       facets = NULL) {
  compact_aes <- purrr::compact(aes)

  aesthetics <- rlang::syms(names(compact_aes))
  names(aesthetics) <- aesthetics
  aes_call <- rlang::call2("aes", !!!aesthetics)

  # Dates and datetimes
  date_call <- function(arg) {
    if (inherits(arg, "POSIXct")) {
      # Format datetime as ISO 8601 string and wrap in as.POSIXct call
      arg <- format(arg, "%Y-%m-%d %H:%M:%S", tz = "UTC")
      arg <- call("as.POSIXct", arg, tz = "UTC")
    } else if (inherits(arg, "Date")) {
      arg <- as.character(arg)
      arg <- call("as.Date", arg)
    }
    arg
  }
  data_cols <- lapply(compact_aes, date_call)

  # Facets
  if (isFALSE(missing(facets)) && length(facets) > 0) {
    if (is.null(names(facets))) {
      stop("facets must be a named list")
    }
    data_cols <- c(data_cols, facets)
  }

  data_call <- rlang::call2("data.frame", !!!data_cols)

  params_list <- purrr::compact(params)

  # Handle textbox namespace - it's from ggtext, not ggplot2
  if (geom == "textbox") {
    geom_to_call <- "ggtext::geom_textbox"
    params_list <- remove_default_params(geom_to_call, params_list)

    # Create a properly namespaced call using :: operator
    rlang::call2(
      call("::", quote(ggtext), quote(geom_textbox)),
      data = data_call,
      mapping = aes_call,
      !!!params_list,
      inherit.aes = FALSE
    )
  } else {
    geom_to_call <- paste0("geom_", geom)
    params_list <- remove_default_params(geom_to_call, params_list)

    rlang::call2(geom_to_call,
      data = data_call,
      mapping = aes_call,
      !!!params_list,
      inherit.aes = FALSE
    )
  }
}
