#' Given the list returned by Shiny as `input$plot_click`, get the
#' facet variables (eg 'cyl') and levels (eg. '"4"') in the plot at the
#' click location
#' @noRd

plot_facets <- function(plot_click) {

  is_panel_var <- grepl("panelvar", names(plot_click), fixed = TRUE)

  panelvars <- names(plot_click)[is_panel_var]

  facet_levels <- plot_click[is_panel_var]

  facet_vars <- plot_click$mapping[names(plot_click$mapping) %in%
                                     panelvars]

  facets <- list(levels = facet_levels, vars = facet_vars)

  facets
}

#' If a facet variable is specified as (eg.) "factor(cyl)" rather than
#' "cyl", we want the annotation to just refer to "cyl". This function takes
#' the facets returned by plot_facets() and takes the text out from between
#' brackets (eg. "factor(cyl)" becomes "cyl"). It then checks that the result
#' is present in the list of columns in the plot data; if so, it modifies the
#' facet var(s) used by ggannotate; if not it leaves them unchanged.
#' @noRd

strip_facet_brackets <- function(facets, built_plot) {
  vars <- facets$vars

  vars_nobrackets <- gsub(".*\\((.*)\\).*", "\\1", vars)

  vars_in_data <- built_plot$layout$facet_params$.possible_columns

  all_modified_vars_in_data <- all(vars_nobrackets %in% vars_in_data)

  if (isTRUE(all_modified_vars_in_data)) {
    new_vars <- as.list(vars_nobrackets)
    names(new_vars) <- names(vars)
    facets$vars <- new_vars
  }

  facets
}
