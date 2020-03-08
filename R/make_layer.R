#' Generate a call to ggplot2::layer() given some aesthetics and parameters
#' Based on ggplot2::annotate()
#' Can handle limiting annotations to (specified) facet level(s)
#'
#' @param geom Geom to annotate, such as "text".
#' @param x Variable to map to x aesthetic
#' @param y Variable to map to y aesthetic
#' @param xend Variable to map to xend (such as for `geom_curve`)
#' @param yend Variable to map to yend (such as for `geom_curve`)
#' @param label Variable to use as label (such as for `geom_text`)
#' @param params List of parameters for geom, such as `list(colour = "black")`
#' @param facet_vars List. The names of variables used to facet the plot,
#' such as list("cyl").
#' @param facet_levels List. The levels of variables you wish to annotation, such
#' as list(4).
#' @param ... Additional aesthetics you wish to pass to geom
#'
#'
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 aes_all
#' @importFrom dplyr bind_cols
#' @importFrom purrr compact
#' @importFrom rlang `:=` call2 syms `!!!`
#' @importFrom stats setNames

make_layer <- function(geom,
                       x = NULL,
                       y = NULL,
                       xend = NULL,
                       yend = NULL,
                       label = NULL,
                       params = NULL,
                       facet_vars = NULL,
                       facet_levels = NULL,
                       ...) {
  aesthetics <- purrr::compact(list(
    x = x, y = y,
    xend = xend, yend = yend,
    label = label
  ))

  aesthetics <- c(aesthetics, list(...))
  aesthetics <- rlang::syms(names(aesthetics))
  names(aesthetics) <- aesthetics
  aes_call <- rlang::call2("aes", !!!aesthetics)

  data_cols <- list(
    x = x, y = y,
    xend = xend, yend = yend,
    label = label
  )

  # Dates
  date_call <- function(arg) {
    if (inherits(arg, "Date")) {
      arg <- as.character(arg)
      arg <- rlang::call2("as.Date", arg)
    }
    arg
  }
  data_cols <- purrr::map(data_cols, date_call)

  # Facets
  facet_levels <- as.list(facet_levels)
  facets <- setNames(facet_levels, facet_vars)

  # Combine data and facets
  data_cols <- c(data_cols, facets)
  data_cols <- purrr::compact(data_cols)
  data_call <- rlang::call2("data.frame", !!!data_cols)

  params_list <- purrr::compact(params)

  geom_to_call <- paste0("geom_", geom)

  rlang::call2(geom_to_call,
    data = data_call,
    mapping = aes_call,
    !!!params_list,
    # stat = "identity",
    # position = "identity",
    inherit.aes = FALSE,
    show.legend = FALSE
  )
}
