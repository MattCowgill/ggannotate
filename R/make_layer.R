#' Generate a call to ggplot2::layer() given some aesthetics and parameters
#' Based on ggplot2::annotate()
#' Can handle limiting annotations to (specified) facet level(s)
#'
#' @param geom Geom to annotate, such as "text".
#' @param aes List of aesthetics with corresponding data values, as in
#' `list(x = 3, y = 30, label = "A label")`.
#' @param params List of parameters for geom, such as `list(colour = "black")`
#' @param facet_vars List. The names of variables used to facet the plot,
#' such as list("cyl").
#' @param facet_levels List. The levels of variables you wish to annotation, such
#' as list(4).
#' @return A call
#'
#' @examples
#' library(ggplot2)
#'
#' base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
#'
#' my_annot_call <- make_layer("text",
#'     aes = list(x = 3, y = 30, label = "A label"),
#'     params = list(col = "orange"))
#'
#' my_annotation <- eval(my_annot_call)
#'
#' base_plot + my_annotation
#'
#' @export
#'
#' @importFrom purrr compact
#' @importFrom rlang call2 syms `!!!`
#' @importFrom stats setNames

make_layer <- function(geom,
                       aes = NULL,
                       params = NULL,
                       facet_vars = NULL,
                       facet_levels = NULL) {

  compact_aes <- purrr::compact(aes)

  aesthetics <- rlang::syms(names(compact_aes))
  names(aesthetics) <- aesthetics
  aes_call <- rlang::call2("aes", !!!aesthetics)

  # Dates
  date_call <- function(arg) {
    if (inherits(arg, "Date")) {
      arg <- as.character(arg)
      arg <- call("as.Date", arg)
    }
    arg
  }
  data_cols <- lapply(compact_aes, date_call)

  # Facets
  if (isFALSE(missing(facet_vars))) {
    facet_levels <- as.list(facet_levels)
    facets <- setNames(facet_levels, facet_vars)
    data_cols <- c(data_cols, facets)
  }

  data_call <- rlang::call2("data.frame", !!!data_cols)

  params_list <- purrr::compact(params)

  geom_to_call <- paste0("geom_", geom)

  params_list <- remove_default_params(geom_to_call, params_list)

  rlang::call2(geom_to_call,
    data = data_call,
    mapping = aes_call,
    !!!params_list,
    inherit.aes = FALSE
  )
}

