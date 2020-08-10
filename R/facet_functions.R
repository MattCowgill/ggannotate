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
  env <- rlang::new_environment(data)
  mask <- rlang::new_data_mask(env)
  mask$.data <- rlang::as_data_pronoun(mask)
  tryCatch(rlang::eval_tidy(facet, mask), ggplot2_missing_facet_var = function(e) NULL)
}

#' This function is a modified copy of ggplot2:::eval_facets()
#' Copied here as that function is non-exported
#' @noRd
ggann_eval_facets <- function(facets, data) {
  vars <- purrr::compact(lapply(facets, ggann_eval_facet, data))
  dplyr::as_tibble(vars)
}

correct_facet_var <- function(facet_var, built_plot) {
  stopifnot(inherits(built_plot, "ggplot_built"))

  plot_data <- built_plot$plot$data

  # If the string facet name can be uniquely matched to the colnames of the
  # plot data, we don't need to do anything to the facet name
  if (!is.null(plot_data[[facet_var]])) {
    matched_facet_var <- facet_var

  # If the facet name cannot be matched, evaluate the facet and then see if the
  # data corresponding to the facet column can be matched to the plot data
  } else {
    facet_quos <- get_facet_quos(built_plot)
    facet_quo <- facet_quos[[facet_var]]
    facet_data_col <- ggann_eval_facet(facet_quo, plot_data)

    matching_cols <- dplyr::select(plot_data,
                            tidyselect::vars_select_helpers$where(function(x) all(x == facet_data_col)))

    if (ncol(matching_cols) == 1) {
      matched_facet_var <- names(matching_cols)
    } else if (ncol(matching_cols) < 1) {
      stop("facet var could not be matched to the plot data")
    } else {
      #stop("more than one column in the plot data matches the facet var")

      var_between_brackets <- sub("^.*?\\((.*)\\)[^)]*$", "\\1", facet_var)


      if (is.null(plot_data[[var_between_brackets]])) {
        stop("facet variable ",facet_var,
             " could not be found"," in the plot data")
      }
      matched_facet_var <- var_between_brackets
    }
  }
  matched_facet_var
}

correct_facet_class <- function(facet_var, facet_level, plot_data) {

  if (inherits(plot_data[[facet_var]], "factor")) {
    call("factor", facet_level)
  } else {
    facet_level
  }

}


get_facet_quos <- function(built_plot) {
  plot_facets <- list(built_plot$layout$facet_params$rows,
                      built_plot$layout$facet_params$cols,
                      built_plot$layout$facet_params$facets)

  plot_facets <- purrr::compact(plot_facets)
  unlist(plot_facets)
}



