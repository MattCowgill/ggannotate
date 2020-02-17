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
#' @param facet_var1 Character. Name of first facet variable.
#' @param facet_level1 Value of first facet variable in the panel you wish to
#' annotate.
#' @param facet_var2 Character. Name of second facet variable.
#' @param facet_level2 Value of second facet variable in the panel you wish to
#' annotate.
#' @param ... Additional aesthetics you wish to pass to `layer()`
#'
#'
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 aes_all
#' @importFrom dplyr bind_cols
#' @importFrom purrr compact
#' @importFrom rlang `:=` call2
#' @importFrom stats setNames

make_layer <- function(geom,
                       x = NULL,
                       y = NULL,
                       xend = NULL,
                       yend = NULL,
                       label = NULL,
                       params = NULL,
                       facet_var1 = NULL,
                       facet_level1 = NULL,
                       facet_var2 = NULL,
                       facet_level2 = NULL,
                       ...) {

  aesthetics <- purrr::compact(list(x = x, y = y,
                             xend = xend, yend = yend,
                             label = label))

  aesthetics <- c(aesthetics, list(...))
  aesthetics <- syms(names(aesthetics))
  names(aesthetics) <- aesthetics
  aes_call <- rlang::call2("aes",!!!aesthetics)

  data_cols <- list(x = x, y = y,
                    xend = xend, yend = yend,
                    label = label)
  facet_vars <- as.list(c(facet_level1, facet_level2))
  facet_vars <- setNames(facet_vars, c(facet_var1, facet_var2))
  data_cols <- c(data_cols, facet_vars)
  data_cols <- purrr::compact(data_cols)
  data_call <- rlang::call2("data.frame",!!!data_cols)

  params_list <- purrr::compact(params)

  call("layer",
       geom = geom,
       data = data_call,
       mapping = aes_call,
       params = params_list,
       stat = "identity",
       position = "identity",
       inherit.aes = FALSE,
       show.legend = FALSE)

}





