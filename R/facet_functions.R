#' Given the list returned by Shiny as `input$plot_click`, get the
#' facet variables (eg 'cyl') and levels (eg. '"4"') in the plot at the
#' click location
#' @noRd

get_facets <- function(plot_click) {

  is_panel_var <- grepl("panelvar", names(plot_click), fixed = TRUE)

  panelvars <- names(plot_click)[is_panel_var]

  facet_levels <- plot_click[is_panel_var]

  facet_vars <- plot_click$mapping[names(plot_click$mapping) %in%
                                     panelvars]

  facets <- list(levels = facet_levels, vars = facet_vars)

  facets
}

#' The input$plot_click object from Shiny returns facet names as characters;
#' this includes (eg.) "factor(cyl)" when the plot includes
#' +facet_wrap(~factor(cyl)). We instead want to return "cyl", but where the
#' faceted variable is a factor, we want the variable in the dataframe returned
#' by ggannoate to be a factor as well. This function takes the clicked facets
#' (input$click modified by ggannotate::get_facets()) and the corresponding
#' built plot and returns facets ready to be included in ggannotate's output
#' @importFrom purrr compact
#' @param clicked_facets list returned by get_facets()
#' @param built_plot ggplot built with ggplot2::ggplot_build()
correct_facets <- function(clicked_facets, built_plot) {
  plot_facets <- list(built_plot$layout$facet_params$rows,
                      built_plot$layout$facet_params$cols,
                      built_plot$layout$facet_params$facets)

  plot_facets <- purrr::compact(plot_facets)
  plot_facets <- unlist(plot_facets)

  plot_data <- built_plot$plot$data

  facet_data <- ggann_eval_facets(plot_facets, plot_data)

  matched_facets <- match_facet(facet_data, plot_data)

  for (panelvar in names(clicked_facets$vars)) {
    clicked_facet <- clicked_facets$vars[[panelvar]]

    matches_click <- matched_facets$facet_name == clicked_facet

    bare_var_name <- unlist(matched_facets$data_name[matches_click])
    var_class <- unlist(matched_facets$class[matches_click])

    clicked_facets$vars[[panelvar]] <- bare_var_name

    if (var_class == "factor") {
      clicked_facets$levels[[panelvar]] <- call("factor", clicked_facets$levels[[panelvar]])
    }
  }

  clicked_facets
}

#' @noRd
match_facet <- function(facet_data, plot_data) {
  matches <- list()

  get_match <- function(facet_data_col, plot_data_col) {
    if (all(facet_data[[facet_data_col]] == plot_data[[plot_data_col]])) {
      c(matches,
        list(facet_name = facet_data_col,
             data_name = plot_data_col,
             class = class(facet_data[[facet_data_col]])))
    }
  }

  facet_data_cols <- colnames(facet_data)
  plot_data_cols <- colnames(plot_data)

  col_combinations <- expand.grid(facet_data_cols = facet_data_cols,
                                  plot_data_cols = plot_data_cols,
                                  stringsAsFactors = FALSE)

  out <- purrr::map2(col_combinations$facet_data_cols,
                     col_combinations$plot_data_cols,
                     get_match)

  out <- purrr::compact(out)
  out <- purrr::transpose(out)
  out
}

#' This function is a modified copy of ggplot2:::eval_facet()
#' Copied here as that function is non-exported
#' @noRd
ggann_eval_facet <- function(facet, data) {
  if (rlang::quo_is_symbol(facet)) {
    facet <- as.character(rlang::quo_get_expr(facet))
    if (facet %in% names(data)) {
      out <- data[[facet]]
    }
    else {
      out <- NULL
    }
    return(out)
  }
  # env <- rlang::new_environment(data)
  # mask <- rlang::new_data_mask(env)
  # mask$.data <- rlang::as_data_pronoun(mask)
  # tryCatch(rlang::eval_tidy(facet, mask), ggplot2_missing_facet_var = function(e) NULL)
  rlang::eval_tidy(facet, data)
}

#' This function is a modified copy of ggplot2:::eval_facets()
#' Copied here as that function is non-exported
#' @noRd
ggann_eval_facets <- function(facets, data) {
  vars <- purrr::compact(lapply(facets, ggann_eval_facet, data))
  dplyr::as_tibble(vars)
}

# correct_facet_var <- function(facet_var, facet_quo, plot_data) {
#
#   # If the string facet name can be uniquely matched to the colnames of the
#   # plot data, we don't need to do anything to the facet name
#   if (!is.null(plot_data[[facet_var]])) {
#     matched_facet_var <- facet_var
#
#   # If the facet name cannot be matched, evaluate the facet and then see if the
#   # data corresponding to the facet column can be matched to the plot data
#   } else {
#     facet_data_col <- ggann_eval_facet(facet_quo, plot_data)
#
#     matching_cols <- dplyr::select(plot_data,
#                             tidyselect::vars_select_helpers$where(function(x) all(x == facet_data_col)))
#
#     if (ncol(matching_cols) == 1) {
#       matched_facet_var <- names(matching_cols)
#     } else if (ncol(matching_cols) < 1) {
#       stop("facet var could not be matched to the plot data")
#     } else {
#       #stop("more than one column in the plot data matches the facet var")
#
#       var_between_brackets <- sub("^.*?\\((.*)\\)[^)]*$", "\\1", facet_var)
#
#
#       if (is.null(plot_data[[var_between_brackets]])) {
#         stop("facet variable ",facet_var,
#              " could not be found"," in the plot data")
#       }
#       matched_facet_var <- var_between_brackets
#     }
#   }
#   matched_facet_var
# }
#
# correct_facet_class <- function(facet_level, facet_quo, plot_data) {
#
#   facet_data <- ggann_eval_facet(facet_quo, plot_data)
#
#   if (inherits(facet_data, "factor")) {
#     call("factor", facet_level)
#   } else {
#     facet_level
#   }
#
# }


get_facet_quos <- function(built_plot) {
  plot_facets <- list(built_plot$layout$facet_params$rows,
                      built_plot$layout$facet_params$cols,
                      built_plot$layout$facet_params$facets)

  plot_facets <- purrr::compact(plot_facets)
  unlist(plot_facets)
}

# correct_facets_2 <- function(facets, built_plot) {
#   facets_out <- facets
#   facets_quos <- get_facet_quos(built_plot)
#   plot_data <- built_plot$plot$data
#
#   for (facet_num in seq_along(facets_out$vars)) {
#     facet_varname <- facets$vars[[facet_num]]
#     facet_quo <- facets_quos[[facet_varname]]
#
#     facets_out$vars[[facet_num]] <- correct_facet_var(facets$vars[[facet_num]],
#                                                       facet_quo,
#                                                       plot_data)
#
#     facets_out$levels[[facet_num]] <- correct_facet_class(facets_out$levels[[facet_num]],
#                                                           facet_quo,
#                                                           plot_data)
#   }
#
#   facets_out
# }

correct_facets_3 <- function(facets, built_plot) {
  facets_out <- facets
  facets_quos <- get_facet_quos(built_plot)
  plot_data <- built_plot$plot$data

  facets_data <- lapply(facets_quos,
                        ggann_eval_facet,
                        data = plot_data)

  for (panelvar in names(facets$vars)) {

    facet <- facets$vars[[panelvar]]
    facet_data <- facets_data[[facet]]

    # Attempt to get matching facet names (eg. 'cyl' rather than 'factor(cyl)')
    # for facets that are not found in the colnames of the data
    if (!facet %in% names(plot_data)) {
      facets_out$vars[[panelvar]] <- match_facet_var(facet,
                                                     facet_data,
                                                     plot_data)
    }

    # Where facet is a factor, the level should be 'factor(x)' rather than 'x'
    facet_factor <- inherits(facets_data[[facet]], "factor")
    if (isTRUE(facet_factor)) {
      facets_out$levels[[panelvar]] <- call("factor",
                                            facets$levels[[panelvar]])
    }
  }

  facets_out

}


match_facet_var <- function(facet, facet_data, plot_data) {

    matching_cols <- purrr::map_lgl(plot_data, function(x) all(x == facet_data))

    matching_cols <- matching_cols[matching_cols == TRUE]

    if (length(matching_cols) == 1) {
      # Unique match, return the name of the matching data column
      matched_facet_var <- names(matching_cols)

    } else if (length(matching_cols) < 1) {
      stop("facet variable `", facet,
           "` could not be found in the plot data")

      # More than 1 match in the data, try and resolve if possible
    } else {
      var_between_brackets <- sub("^.*?\\((.*)\\)[^)]*$", "\\1", facet)

      if (!var_between_brackets %in% names(matching_cols)) {
        stop("facet variable `", facet,
             "` could not be found in the plot data")
      }
      matched_facet_var <- var_between_brackets
    }

  matched_facet_var
}
