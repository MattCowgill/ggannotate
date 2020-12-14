#' ggannotate
#' @name ggannotate
#'
#' @param plot A ggplot2 object. Default is `ggplot2::last_plot()`.
#'
#' @examples
#'
#' \dontrun{
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'   geom_point()
#'
#' ggannotate(p)
#' }
#'
#' @export
#' @import shiny
#' @import ggplot2
#' @importFrom rlang expr exec enquo get_expr expr_deparse parse_expr
#' @importFrom dplyr case_when if_else
#' @importFrom miniUI miniPage
#' @importFrom clipr write_clip
#'

ggannotate <- function(plot = last_plot()) {
  if (!interactive()) {
    stop("`ggannotate` only works in interactive sessions.")
  }

  stopifnot(inherits(plot, "gg"))

  built_base_plot <- ggplot2::ggplot_build(plot)

  if (inherits(built_base_plot$layout$coord, "CoordPolar")) {
    stop("ggannotate() does not work with polar coordinates.")
  }

  # Shiny server ------

  ggann_server <- function(input, output, session) {
    observeEvent(input$done, shiny::stopApp())

    user_input <- reactiveValues(
      x = NULL,
      y = NULL,
      x_dbl = NULL,
      y_dbl = NULL,
      xmin = NULL,
      xmax = NULL,
      ymin = NULL,
      ymax = NULL
    )

    # Check whether axes are flipped
    flipped_coords <- ggplot2::summarise_coord(built_base_plot)$flip

    # Check whether axes are dates
    axis_classes <- check_if_date(built_base_plot)

    # Get information about facets in plot
    facet_characteristics <- get_facet_characteristics(built_base_plot)

    # Get information about selected geom and annotation layer
    annot_layer <- reactive(input$annot_layer)
    selected_geom <- reactive(input$geom)

    geom_fn <- reactive({
      switch(selected_geom(),
        "text"  = ggplot2::GeomText,
        "label" = ggplot2::GeomLabel,
        "curve" = ggplot2::GeomCurve,
        "rect" = ggplot2::GeomRect
      )
    })

    # Get vector of aesthetics known to selected geom -----
    known_aes <- reactive({
      geom_fn()$aesthetics()
    })

    req_aes <- reactive({
      geom_fn()$required_aes
    })

    # Observe plot interaction -----
    observeEvent(input$plot_click, {
      facets <- get_facets(input$plot_click)
      facets <- correct_facets(facets, facet_characteristics)
      user_input$facet_vars <- facets$vars
      user_input$facet_levels <- facets$levels


      corrected_scales <- correct_scales(
        input$plot_click,
        axis_classes,
        flipped_coords
      )

      user_input$x <- corrected_scales$x
      user_input$y <- corrected_scales$y
    })

    observeEvent(input$plot_dblclick, {
      corrected_scales <- correct_scales(
        input$plot_dblclick,
        axis_classes,
        flipped_coords
      )

      user_input$x_dbl <- corrected_scales$x
      user_input$y_dbl <- corrected_scales$y
    })

    observeEvent(input$plot_brush, {
      facets <- get_facets(input$plot_brush)
      facets <- correct_facets(facets, facet_characteristics)
      user_input$facet_vars <- facets$vars
      user_input$facet_levels <- facets$levels

      corrected_scales <- correct_scales(
        input$plot_brush,
        axis_classes,
        flipped_coords
      )

      user_input$xmin <- corrected_scales$xmin
      user_input$xmax <- corrected_scales$xmax
      user_input$ymin <- corrected_scales$ymin
      user_input$ymax <- corrected_scales$ymax
    })


    # Create list of parameters based on user input ----
    params_list <- reactive({
      user_arrow <- safe_arrow(
        angle = input$arrow_angle,
        length = input$arrow_length,
        ends = "last",
        type = "closed"
      )

      user_label_padding <- safe_unit(input$label.padding, "lines")
      user_label_r <- safe_unit(input$label.r, "lines")

      size <- ifelse(selected_geom() %in% c("text", "label"),
        # Default ggplot2 size is 3.88 = 11.03967 points
        ifelse(input$size == 11,
               3.88,
               input$size / ggplot2::.pt),
        input$size
      )

      fontface <- case_when(
        input$fontface == "plain" ~ 1,
        input$fontface == "bold" ~ 2,
        input$fontface == "italic" ~ 3,
        input$fontface == "bold.italic" ~ 4,
        TRUE ~ NA_real_
      )

      user_alpha <- NA
      # user_alpha <- ifelse(selected_geom() == "rect", input$alpha, NA)

      params <- list(
        size = size,
        angle = input$angle,
        lineheight = input$lineheight,
        hjust = input$hjust,
        vjust = input$vjust,
        colour = input$colour,
        fill = input$fill,
        family = input$font,
        fontface = fontface,
        label.padding = user_label_padding,
        label.size = input$label.size,
        label.r = user_label_r,
        curvature = input$curvature,
        arrow = user_arrow,
        alpha = user_alpha
      )

      # Remove parameters from the list if they are not known by the geom
      known_params <- switch(selected_geom(),
        "text" = c(known_aes()),
        "label" = c(
          known_aes, "label.padding", "label.r",
          "label.size"
        ),
        "curve" = c(
          known_aes, "curvature", "angle",
          "arrow", "arrow.fill", "lineend"
        ),
        "rect" = c(known_aes()),
      )
      params <- params[names(params) %in% known_params]

      purrr::compact(params)
    })

    # Create list of aesthetics based on user input ----
    aes_list <- reactive({
      annot <- input$annotation
      annot_no_esc <- gsub("\\n", "\n", annot, fixed = TRUE)

      aes <- list(
        x = user_input$x,
        y = user_input$y,
        xend = user_input$x_dbl,
        yend = user_input$y_dbl,
        xmin = user_input$xmin,
        xmax = user_input$xmax,
        ymin = user_input$ymin,
        ymax = user_input$ymax,
        label = annot_no_esc
      )

      aes <- aes[names(aes) %in% known_aes()]

      aes <- purrr::compact(aes)

      aes
    })

    # Create list of facets based on user input ----

    facets_list <- reactive({
      setNames(
        user_input$facet_levels,
        user_input$facet_vars
      )
    })


    # Collect inputs in a list of lists ----
    this_layer <- reactive({
      list(
        geom = selected_geom(),
        aes = aes_list(),
        params = params_list(),
        facets = facets_list()
      ) %>%
        purrr::compact()
    })

    all_layers <- reactiveValues()

    observe({
      all_layers[[as.character(annot_layer())]] <- this_layer()
    })

    combined_layers <- reactive({
      combine_layers(all_layers)
    })

    annot_calls <- reactive({
      raw_calls <- purrr::map(
        .x = combined_layers(),
        .f = ~ make_layer(
          geom = .x$geom,
          aes = .x$aes,
          params = .x$params,
          facets = .x$facets
        )
      )

      eval_calls <- purrr::map(raw_calls, eval)

      req_aes_present <- purrr::map_lgl(eval_calls, has_req_aes)

      raw_calls[req_aes_present]
    })

    output$instruction <- renderText({
      dplyr::case_when(
        selected_geom() == "text" ~ "Click where you want to place your annotation",
        selected_geom() == "label" ~ "Click where you want to place your label",
        selected_geom() == "curve" ~ "Click where you want your line to begin and double-click where it should end",
        selected_geom() == "rect" ~ "Click and drag to draw and adjust the rectangle, then click once anywhere else to set it",
        TRUE ~ "No instruction defined for geom"
      )
    })

    output$plot <- renderPlot({
      built_base_plot$plot +
        purrr::map(annot_calls(), eval)
    })

    output$rendered_plot <- renderUI({
      size_units <- input$size_units

      plot_width <- paste0(input$plot_width, size_units)
      plot_height <- paste0(input$plot_height, size_units)

      plotOutput("plot",
        click = "plot_click",
        dblclick = "plot_dblclick",
        brush = shiny::brushOpts(id = "plot_brush"),
        width = plot_width,
        height = plot_height
      )
    })

    output$geom_opts <- renderUI({
      switch(selected_geom(),
        "text"   = text_ui,
        "label"  = label_ui,
        "curve"  = curve_ui,
        "rect"  = rect_ui
      )
    })

    observeEvent(input$copy_button, {
      callstring <- calls_to_string(annot_calls())
      clipr::write_clip(callstring, object_type = "character")
      ggplot2::set_last_plot(built_base_plot$plot)
      stopApp()
    })

    output$code_output <- renderPrint({
      # calls_to_string(annot_calls())
      annot_calls()
    })
  }

  ggann_app <- shiny::shinyApp(ggann_ui, ggann_server)

  shiny::runGadget(
    app = ggann_app,
    viewer = shiny::dialogViewer("Annotate plot with ggannotate",
      width = 1300,
      height = 780
    ),
    stopOnCancel = TRUE
  )
}
