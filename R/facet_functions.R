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

