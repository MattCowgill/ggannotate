#' Given the list returned by Shiny as `input$plot_click` and similar, get the
#' facet variables (eg 'cyl') and levels (eg. '"4"') in the plot at the
#' click location and return as a list
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

#' The `input$plot_click`` object (and similar) from Shiny returns
#' facet names as characters; this includes (eg.) "factor(cyl)"
#' when the plot includes +facet_wrap(~factor(cyl)).
#' We instead want to return "cyl". Where the faceted variable is a factor,
#' we want the variable in the dataframe returned
#' by ggannoate to be a factor as well. This function helps us do that.
#'
#' This function returns a list with one
#' element per facet. Each element of the list is a list with three elements:
#' 'facet_name' is the string name of the facet (such as "factor(cyl)"),
#' 'data_name' is the name of the column from the data that facet_name refers to
#' (such as "cyl"), and 'facet_factor' is a logical, TRUE if the column is a
#' factor.
#' @param built_plot a ggplot2 plot built with `ggplot2::ggplot_built()`
#' @noRd
get_facet_characteristics <- function(built_plot) {
  facets_quos <- get_facet_quos(built_plot)
  facets_names <- names(facets_quos)

  plot_data <- built_plot$plot$data

  facets_data <- lapply(facets_quos,
                        ggann_eval_facet,
                        data = plot_data)

  facets_out <- list()

  for (facet_name in facets_names) {
    facet_data <- facets_data[[facet_name]]

    # Attempt to get matching facet names (eg. 'cyl' rather than 'factor(cyl)')
    # for facets that are not found in the colnames of the data
    if (!facet_name %in% names(plot_data)) {
      data_name <- match_facet_var(facet_name,
                                   facet_data,plot_data)
    } else {
      data_name <- facet_name
    }

    # Where facet is a factor, the level should be 'factor(x)' rather than 'x'
    facet_factor <- inherits(facets_data[[facet_name]], "factor")

    facet_list <- list(facet = list(facet_name = facet_name,
                                    data_name = data_name,
                                    facet_factor = facet_factor))

    names(facet_list) <- facet_name

    facets_out <- c(facets_out, facet_list)
  }

  facets_out
}

#' @param clicked_facets A list returned by `get_facets()`
#' @param facet_characteristics A list returned by `get_facet_characteristics()`
#' @noRd
correct_facets <- function(clicked_facets, facet_characteristics) {
  facets_out <- clicked_facets

  if (length(clicked_facets$vars) > 0) {
    for (panelvar in names(clicked_facets$vars)) {
      facet_name <- clicked_facets$vars[[panelvar]]

      facets_out$vars[[panelvar]] <- facet_characteristics[[facet_name]]$data_name

      facet_factor <- facet_characteristics[[facet_name]]$facet_factor

      if (isTRUE(facet_factor)) {
        facets_out$levels[[panelvar]] <- call("factor",
                                              facets_out$levels[[panelvar]])
      }
    }
  }
  facets_out
}



#' This function is a simplified version of ggplot2:::eval_facet()
#' Copied here as that function is non-exported
#' @param facet A facet quosure, obtained through `get_facet_quos(built_plot)`
#' @param data plot data, as in `build_plot$plot$data`
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

  rlang::eval_tidy(facet, data)
}


#' Get list of facet quosures from built ggplot
#' @noRd
get_facet_quos <- function(built_plot) {
  plot_facets <- list(built_plot$layout$facet_params$rows,
                      built_plot$layout$facet_params$cols,
                      built_plot$layout$facet_params$facets)

  plot_facets <- purrr::compact(plot_facets)
  unlist(plot_facets)
}

#' If facet name (eg. 'factor(cyl)'), try and find a unique match between the
#' facet data and the data in the plot. If one is found, return it, if not
#' error.
#' @param facet facet as string, such as "cyl" or "factor(cyl)"
#' @param facet_data vector; obtained by evaluating facet quosure in the plot
#' environment using `ggann_eval_facet()`
#' @param plot_data dataframe from built plot, `built_plot$plot$data`
#' @noRd

match_facet_var <- function(facet, facet_data, plot_data) {
    matching_cols <- purrr::map_lgl(plot_data,
                                    function(x) all(as.character(x) == as.character(facet_data)))

    matching_cols <- matching_cols[matching_cols == TRUE]

    if (length(matching_cols) == 1) {
      # Unique match, return the name of the matching data column
      matched_facet_var <- names(matching_cols)

    # } else if (length(matching_cols) < 1) {
    #   stop("facet variable `", facet,
    #        "` could not be found in the plot data")

      # More than 1 match in the data, try and resolve if possible
    } else {
      # First strip brackets, so that eg. 'factor(cyl)' becomes 'cyl' and
      # look for matches
      var_between_brackets <- sub("^.*?\\((.*)\\)[^)]*$", "\\1", facet)

      if (!var_between_brackets %in% names(plot_data)) {
        # Now look before commas, so that 'factor(cyl, levels = c(1, 2, 3))
        # becomes 'cyl' and look for matches
        var_before_comma <- sub(",.*", "\\1", var_between_brackets)

        if (!var_before_comma %in% names(plot_data)) {
          stop("facet variable `", facet,
               "` could not be found in the plot data")
        }

        matched_facet_var <- var_before_comma
      } else {
        matched_facet_var <- var_between_brackets
      }

    }

  matched_facet_var
}

