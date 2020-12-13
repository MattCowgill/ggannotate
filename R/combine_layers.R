#' Take lists corresponding to several annotations, combine annotations
#' that share parameters and geom, and return a list-of-lists.
#' @name combine_layers
#' @param ... lists to combine. Each list should have three elemnts:
#' \itemize{
#'   \item{`geom`}{length-one character vector such as `"text"`, or `"point"`}
#'   \item{`aes`}{named list containing variable-value mappings,
#'   such as `list(x = 3, y = 40)`)}
#'   \item{`param`}{named list containing parameter names and values, such as
#' `list(colour = "black")`}
#'
#' }
#'
#'
#' @return A list of lists. Any supplied lists that share a geom and parameters
#' will be combined, so the length of the returned list will not necessarily
#' equal the number of lists supplied to `...`.
#'
#' @examples
#' # Supply list(s) with three elements: geom (length 1 character vector),
#' # aes (list), and params (list).
#' library(ggplot2)
#'
#' layer_1 <- list(geom = "text",
#'                 aes = list(x = 3, y = 30, label = "Some text"),
#'                 params = list(colour = "red"))
#'
#' layer_2 <- list(geom = "text",
#'                 aes = list(x = 4, y = 35, label = "Some other text"),
#'                 params = list(colour = "red"))
#'
#' layer_3 <- list(geom = "point",
#'                 aes = list(x = 3, y = 40))
#'
#' layer_4 <- list(geom = "text",
#'                 aes = list(x = 4, y = 45, label = "Some more text"),
#'                 params = list(colour = "black"))
#'
#' # combine_layers() combined layers 1 and 2 as they share a geom and params.
#'
#' layers <- combine_layers(layer_1, layer_2, layer_3, layer_4)
#'
#' # The resulting list can be used to create ggplot2 annotations
#'
#' annots <-   purrr::map(layers,
#'                        ~make_layer(geom = .x$geom,
#'                                    aes = .x$aes,
#'                                    params = .x$params) %>%
#'                          eval())
#'
#' ggplot2::ggplot() +
#'   annots
#'
#' @export
#' @importFrom rlang .data

combine_layers <- function(...) {
  if (missing(...)) {
    stop("Must supply list(s) to ...")
  }

  lists <- list(...)

  each_element_is_list <- all(purrr::map_lgl(lists, is.list))

  stopifnot(each_element_is_list)

  x <- dplyr::tibble(layer = lists)
  x <- tidyr::unnest_wider(x, .data$layer)
  x <- x %>%
    dplyr::group_by(.data$geom, .data$params) %>%
    dplyr::summarise(aes = list(.data$aes), .groups = "drop") %>%
    dplyr::mutate(annot = dplyr::row_number())

  x <- split(x, x$annot)

  create_aes_out <- function(nested_tib) {
    list_out <- nested_tib %>%
      dplyr::select(.data$aes) %>%
      tidyr::unnest_longer(.data$aes) %>%
      tidyr::unnest_wider(.data$aes) %>%
      as.list()

    list_out
  }

  aes <- purrr::map(x, create_aes_out)

  params <- purrr::map(x, ~purrr::flatten(.x[["params"]]))

  geoms <- purrr::map(x, ~.x[["geom"]])

  out <- list(aes = aes,
       params = params,
       geom = geoms)

  purrr::transpose(out)
}



