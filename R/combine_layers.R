#' Take lists corresponding to several annotations, combine annotations
#' that share parameters and geom, and return a list-of-lists.
#' @name combine_layers
#' @param lists List of lists to combine. Each sub-list should have
#' four elements:
#' \itemize{
#'   \item{`geom`}{length-one character vector such as `"text"`, or `"point"`}
#'   \item{`aes`}{named list containing variable-value mappings,
#'   such as `list(x = 3, y = 40)`)}
#'   \item{`param`}{named list containing parameter names and values, such as
#' `list(colour = "black")`}
#'   \item{`facets`}{named list containing facet variable-value pairs, such as
#'   `list(cyl = 4)`}
#'
#' }
#'
#'
#' @return A list of lists. Any supplied lists that share a geom and parameters
#' will be combined, so the length of the returned list will not necessarily
#' equal the number of lists supplied to `lists`.
#'
#' @examples
#' library(ggplot2)
#'
#' layer_1 <- list(
#'   geom = "text",
#'   aes = list(x = 3, y = 30, label = "Some text"),
#'   facets = list(cyl = 4),
#'   params = list(colour = "red")
#' )
#'
#' layer_2 <- list(
#'   geom = "text",
#'   aes = list(x = 4, y = 35, label = "Some other text"),
#'   params = list(colour = "red")
#' )
#'
#' layer_3 <- list(
#'   geom = "point",
#'   aes = list(x = 3, y = 40)
#' )
#'
#' layer_4 <- list(
#'   geom = "text",
#'   aes = list(x = 4, y = 45, label = "Some more text"),
#'   params = list(colour = "black")
#' )
#'
#' lists <- list(layer_1, layer_2, layer_3, layer_4)
#'
#' # combine_layers() combined layers 1 and 2 as they share a geom and params.
#'
#' layers <- combine_layers(lists)
#'
#' # The resulting list can be used to create ggplot2 annotations
#'
#' annots <- purrr::map(
#'   layers,
#'   ~ make_layer(
#'     geom = .x$geom,
#'     aes = .x$aes,
#'     params = .x$params
#'   ) %>%
#'     eval()
#' )
#'
#' ggplot2::ggplot() +
#'   annots
#' @noRd
#' @importFrom rlang .data

combine_layers <- function(lists) {
  if (missing(lists)) {
    stop("Must supply list of lists")
  }

  if (inherits(lists, "reactivevalues")) {
    lists <- shiny::reactiveValuesToList(lists)
  }

  each_element_is_list <- all(purrr::map_lgl(
    lists,
    is.list
  ))

  stopifnot(each_element_is_list)

  x <- dplyr::tibble(layer = lists)
  x <- tidyr::unnest_wider(x, .data$layer)

  if (is.null(x[["facets"]])) {
    x <- dplyr::group_by(x, .data$geom, .data$params)
  } else {
    x <- dplyr::group_by(x, .data$geom, .data$params, .data$facets)
  }

  x <- x %>%
    dplyr::summarise(aes = list(.data$aes), .groups = "drop") %>%
    dplyr::mutate(annot = dplyr::row_number())

  x <- split(x, x$annot)

  create_aes_out <- function(split_tib) {
    aes_col <- split_tib %>%
      dplyr::select(.data$aes) %>%
      tidyr::unnest_longer(.data$aes)

    if (all(is.na(aes_col))) {
      list_out <- list(aes = NULL)
    } else {
      list_out <- aes_col %>%
        tidyr::unnest_wider(.data$aes) %>%
        as.list()
    }

    list_out
  }

  aes <- purrr::map(x, create_aes_out)

  params <- purrr::map(x, ~ purrr::flatten(.x[["params"]]))

  geoms <- purrr::map(x, ~ .x[["geom"]])

  add_facet_or_flatten <- function(list) {
    if (is.null(list[["facets"]])) {
      out <- list()
    } else {
      out <- purrr::flatten(list[["facets"]])
    }
    out
  }
  facets <- purrr::map(x, add_facet_or_flatten)

  out <- list(
    aes = aes,
    params = params,
    facets = facets,
    geom = geoms
  )

  purrr::transpose(out)
}

safely_combine_layers <- purrr::safely(combine_layers)
