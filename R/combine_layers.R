#' Take lists corresponding to several annotations, combine annotations
#' that share parameters and geom, and return a list-of-lists.
#' @name combine_layers
#' @param lists List of lists to combine. Each sub-list should have
#' at least these two elements:
#' \itemize{
#'   \item `geom`: length-one character vector such as `"text"`, or `"point"`
#'   \item `aes`: named list containing variable-value mappings,
#'   such as `list(x = 3, y = 40)`
#' }
#'
#' It can also have two optional additional elements:
#' \itemize{
#'   \item `param`: named list containing parameter names and values, such as
#'   `list(colour = "black")`
#'   \item `facets`: named list containing facet variable-value pairs, such as
#'   `list(cyl = 4)`
#' }
#'
#'
#' @return A list of lists. Each sub-list will have `geom`, `aes`, `params`,
#' and `facet` elements.
#'
#' Any supplied lists that share a geom, parameters, and facet variable + level
#' will be combined, so the length of the returned list will not necessarily
#' equal the number of lists supplied to `lists`.
#'
#' The order of the
#' sub-lists in the returned list may also differ from the order
#' in the supplied list.
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
#'   facets = list(cyl = 4),
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
combine_layers <- function(lists) {
  if (missing(lists)) {
    stop("Must supply list of lists")
  }

  if (length(lists) == 0) {
    stop("Must supply list of lists")
  }

  if (inherits(lists, "reactivevalues")) {
    lists <- shiny::reactiveValuesToList(lists)
  }

  check_element_is_layer <- function(element) {
    element_is_list <- is.list(element)
    has_expected_sub_elements <- all(c("geom", "aes") %in% names(element))
    has_no_unexpected_sub_elements <- all(
      names(element) %in%
        c(
          "geom",
          "aes",
          "facets",
          "params"
        )
    )

    aes_is_list <- is.list(element[["aes"]])

    all(
      element_is_list,
      has_expected_sub_elements,
      aes_is_list,
      has_no_unexpected_sub_elements
    )
  }

  each_element_is_layer <- all(purrr::map_lgl(
    lists,
    check_element_is_layer
  ))

  stopifnot(each_element_is_layer)

  x <- dplyr::tibble(layer = lists)
  x <- tidyr::unnest_wider(x, "layer")

  # Group by facet variable names (not values) so annotations in different
  # panels of the same facet variable can be combined into one geom call.
  # Ensure the facets column exists even when no layers have facets.
  if (!"facets" %in% names(x)) {
    x$facets <- vector("list", nrow(x))
  }
  x$facet_var_names <- purrr::map_chr(x$facets, function(f) {
    if (is.null(f) || length(f) == 0) {
      ""
    } else {
      paste(sort(names(f)), collapse = ",")
    }
  })

  x <- dplyr::group_by(
    x,
    dplyr::across(!dplyr::one_of(c("aes", "facets")))
  )

  x <- x |>
    dplyr::summarise(
      aes = list(.data[["aes"]]),
      facets = list(.data[["facets"]]),
      .groups = "drop"
    ) |>
    dplyr::mutate(annot = dplyr::row_number())

  x <- split(x, x$annot)

  create_aes_out <- function(split_tib) {
    aes_col <- split_tib %>%
      dplyr::select("aes") %>%
      tidyr::unnest_longer("aes")

    if (all(is.na(aes_col))) {
      list_out <- list(aes = NULL)
    } else {
      list_out <- aes_col %>%
        tidyr::unnest_wider("aes") %>%
        as.list()
    }

    list_out
  }

  aes <- purrr::map(x, create_aes_out)

  geoms <- purrr::map(x, ~ .x[["geom"]])

  add_element_or_flatten <- function(list, element) {
    if (is.null(list[[element]])) {
      out <- list()
    } else {
      out <- purrr::flatten(list[[element]])
    }
    out
  }

  params <- purrr::map(x, add_element_or_flatten, element = "params")

  facets <- purrr::map(x, function(xi) {
    facet_col <- xi[["facets"]]
    if (is.null(facet_col)) {
      return(list())
    }
    facet_lists <- purrr::compact(facet_col[[1]])
    if (length(facet_lists) == 0) {
      return(list())
    }
    all_names <- unique(unlist(lapply(facet_lists, names)))
    result <- lapply(all_names, function(nm) {
      unlist(lapply(facet_lists, function(fl) fl[[nm]]))
    })
    names(result) <- all_names
    result
  })

  out <- list(
    aes = aes,
    params = params,
    facets = facets,
    geom = geoms
  )

  purrr::transpose(out)
}

safely_combine_layers <- purrr::safely(combine_layers)
