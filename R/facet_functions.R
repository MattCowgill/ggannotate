#' Given the list returned by Shiny as `input$plot_click`, count the
#' number of facet variables in the plot to which `input$plot_click` pertains
#' @importFrom purrr map_lgl
#' @noRd

count_facet_vars <- function(plot_click) {

  non_null_var <- function(var_num) {
    var_name <- paste0("panelvar", var_num)
    ifelse(is.null(plot_click[[var_name]]),
                   FALSE,
                   TRUE)
  }

  results <- purrr::map_lgl(1:10, non_null_var)

  sum(results)

}


list_facet_vars <- function(plot_click, num_facet_vars) {
  get_facet_var <- function(plot_click, panelvar) {
    plot_click$mapping[[panelvar]]
  }

  panelvars <- paste0("panelvar", seq.int(1, num_facet_vars))

  map(panelvars, get_facet_var, plot_click = plot_click)

}

list_facet_levels <- function(plot_click, num_facet_vars) {
  get_facet_level <- function(plot_click, panelvar) {
    plot_click[[panelvar]]
  }

  panelvars <- paste0("panelvar", seq.int(1, num_facet_vars))

  map(panelvars, get_facet_level, plot_click = plot_click)
}

