#' Generate a call to ggplot2::layer() given some aesthetics and parameters
#' Based on ggplot2::annotate()
#' Can handle limiting annotations to (specified) facet level(s)
#'
#' @param geom Geom to annotate, such as "text".
#' @param x Variable to map to x aesthetic
#' @param y Variable to map to y aesthetic
#'
#'
#' @keywords internal
#' @importFrom tibble as_tibble tibble
#' @importFrom ggplot2 aes_all
#' @importFrom dplyr bind_cols
#' @importFrom purrr compact
#' @importFrom rlang `:=`

make_layer <- function(geom,
                       x = NULL,
                       y = NULL,
                       xend = NULL,
                       yend = NULL,
                       label = NULL,
                       params = NULL,
                       annotate_all_facets = FALSE,
                       facet_var1 = NULL,
                       facet_level1 = NULL,
                       facet_var2 = NULL,
                       facet_level2 = NULL,
                       ...,
                       na.rm = FALSE) {

  aesthetics <- compact(list(x = x, y = y,
                             xend = xend, yend = yend,
                             label = label))

  aesthetics <- c(aesthetics, list(...))
  data <- tibble::as_tibble(aesthetics)
  aesthetics <- ggplot2::aes_all(names(data))

  # Create new col(s) corresponding to facet(s)
  if (!is.null(facet_var1)) {
    if(is.null(facet_level1)) {
      stop("`facet_var` specified with no `facet_level`")
    }
    facet_df1 <- tibble::tibble(!!facet_var1 := facet_level1)
  }

  if (isFALSE(annotate_all_facets)) {
    if (!is.null(facet_var2)) {
      if(is.null(facet_level2)) {
        stop("`facet_var` specified with no `facet_level`")
      }
      facet_df2 <- tibble::tibble(!!facet_var2 := facet_level2)
    }

    if (!is.null(facet_var1) && !is.null(facet_var2)) {
      facet_df <- dplyr::bind_cols(facet_df1, facet_df2)
    } else if (!is.null(facet_var1)) {
      facet_df <- facet_df1
    } else if(!is.null(facet_var2)) {
      facet_df <- facet_df2
    }
  }

  if(exists("facet_df")) {
    data <- dplyr::bind_cols(data, facet_df)
  }

  params_list <- purrr::compact(c(list(na.rm = na.rm),
                           params))

  call("layer",
       geom = geom,
        params = params_list,
        stat = "identity",
        position = "identity",
        data = data,
        mapping = aesthetics,
        inherit.aes = FALSE,
        show.legend = FALSE)

}





