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

  if (is_polar_coord(built_base_plot)) {
    stop("ggannotate() does not work with polar coordinates.")
  }

  # Shiny server ------

  ggann_server <- function(input, output, session) {
    observeEvent(input$done, shiny::stopApp())

    user_input <- reactiveValues()

    # Check whether axes are flipped
    flipped_coords <- get_flipped_coords(built_base_plot)

    # Check whether axes are dates
    axis_classes <- check_if_date(built_base_plot)

    # Get information about facets in plot
    facet_characteristics <- get_facet_characteristics(built_base_plot)

    # Get panel params for coordinate conversion (needed when Shiny returns normalized coords)
    panel_params <- get_panel_params(built_base_plot)

    # Get axis ranges for intelligent rounding of click coordinates
    x_range <- panel_params$x.range
    y_range <- panel_params$y.range

    # Get information about selected geom and annotation layer
    annot_layer <- reactive(input$annot_layer)
    selected_geom <- reactive(input$geom)

    geom_fn <- reactive({
      switch(
        selected_geom(),
        "text" = ggplot2::GeomText,
        "label" = ggplot2::GeomLabel,
        "curve" = ggplot2::GeomCurve,
        "rect" = ggplot2::GeomRect,
        "textbox" = ggtext::GeomTextBox
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
      click_data <- input$plot_click

      # Check if we need to infer facets (normalized coords + missing panelvar)
      is_normalized <- coords_are_normalized(click_data, panel_params)
      needs_facet_inference <- is_normalized &&
        length(get_facets(click_data)$vars) == 0 &&
        length(facet_characteristics) > 0

      # Save normalized coords for facet inference before converting
      if (is_normalized) {
        norm_x <- click_data$x
        norm_y <- click_data$y
        # Convert whole-plot coords to within-panel coords for faceted plots
        panel_coords <- convert_to_panel_coords(norm_x, norm_y, built_base_plot)
        click_data$x <- panel_coords$x
        click_data$y <- panel_coords$y
        click_data <- normalize_to_data_coords(click_data, panel_params)
      }

      if (needs_facet_inference) {
        # Infer facets from normalized position when Shiny doesn't provide them
        facets <- infer_facet_from_normalized_coords(
          norm_x,
          norm_y,
          built_base_plot
        )
        facets <- correct_facets(facets, facet_characteristics)
      } else {
        facets <- get_facets(click_data)
        facets <- correct_facets(facets, facet_characteristics)
      }
      user_input$facet_vars <- facets$vars
      user_input$facet_levels <- facets$levels

      corrected_scales <- correct_scales(
        click_data,
        axis_classes,
        flipped_coords
      )

      user_input$x <- round_to_range(corrected_scales$x, x_range)
      user_input$y <- round_to_range(corrected_scales$y, y_range)
    })

    observeEvent(input$plot_dblclick, {
      click_data <- input$plot_dblclick

      # Convert normalized (0-1) coordinates to data coordinates if needed
      if (coords_are_normalized(click_data, panel_params)) {
        # Convert whole-plot coords to within-panel coords for faceted plots
        panel_coords <- convert_to_panel_coords(
          click_data$x,
          click_data$y,
          built_base_plot
        )
        click_data$x <- panel_coords$x
        click_data$y <- panel_coords$y
        click_data <- normalize_to_data_coords(click_data, panel_params)
      }

      corrected_scales <- correct_scales(
        click_data,
        axis_classes,
        flipped_coords
      )

      user_input$x_dbl <- round_to_range(corrected_scales$x, x_range)
      user_input$y_dbl <- round_to_range(corrected_scales$y, y_range)
    })

    observeEvent(input$plot_brush, {
      brush_data <- input$plot_brush

      # Convert normalized (0-1) coordinates to data coordinates if needed
      # For brush, check using xmin/ymin instead of x/y
      is_normalized <- FALSE
      if (
        !is.null(brush_data$xmin) &&
          !is.null(brush_data$ymin) &&
          !is.null(panel_params)
      ) {
        brush_check <- list(x = brush_data$xmin, y = brush_data$ymin)
        is_normalized <- coords_are_normalized(brush_check, panel_params)
      }

      # Check if we need to infer facets
      needs_facet_inference <- is_normalized &&
        length(get_facets(brush_data)$vars) == 0 &&
        length(facet_characteristics) > 0

      # Save normalized coords for facet inference before converting
      if (is_normalized) {
        # Use center of brush for facet inference
        norm_x <- (brush_data$xmin + brush_data$xmax) / 2
        norm_y <- (brush_data$ymin + brush_data$ymax) / 2
        # Convert whole-plot coords to within-panel coords for faceted plots
        panel_min <- convert_to_panel_coords(
          brush_data$xmin,
          brush_data$ymin,
          built_base_plot
        )
        panel_max <- convert_to_panel_coords(
          brush_data$xmax,
          brush_data$ymax,
          built_base_plot
        )
        brush_data$xmin <- panel_min$x
        brush_data$ymin <- panel_min$y
        brush_data$xmax <- panel_max$x
        brush_data$ymax <- panel_max$y
        brush_data <- normalize_to_data_coords(brush_data, panel_params)
      }

      if (needs_facet_inference) {
        facets <- infer_facet_from_normalized_coords(
          norm_x,
          norm_y,
          built_base_plot
        )
        facets <- correct_facets(facets, facet_characteristics)
      } else {
        facets <- get_facets(brush_data)
        facets <- correct_facets(facets, facet_characteristics)
      }
      user_input$facet_vars <- facets$vars
      user_input$facet_levels <- facets$levels

      corrected_scales <- correct_scales(
        brush_data,
        axis_classes,
        flipped_coords
      )

      user_input$xmin <- round_to_range(corrected_scales$xmin, x_range)
      user_input$xmax <- round_to_range(corrected_scales$xmax, x_range)
      user_input$ymin <- round_to_range(corrected_scales$ymin, y_range)
      user_input$ymax <- round_to_range(corrected_scales$ymax, y_range)
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
      user_box_padding <- safe_unit(input$`box.padding`, "pt")
      user_width <- safe_unit(input$width, "inch")

      size <- ifelse(
        selected_geom() %in% c("text", "label", "textbox"),
        # Default ggplot2 size is 3.88 = 11.03967 points
        # We want to match this, which using .pt doesn't quite do
        round(input$size / 2.835052, 2),
        input$size
      )

      fontface <- case_when(
        input$fontface == "plain" ~ 1,
        input$fontface == "bold" ~ 2,
        input$fontface == "italic" ~ 3,
        input$fontface == "bold.italic" ~ 4,
        TRUE ~ NA_real_
      )

      user_alpha <- ifelse(
        selected_geom() == "rect" &&
          !is.null(input$alpha),
        input$alpha,
        NA
      )

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
        alpha = user_alpha,
        box.padding = user_box_padding,
        width = user_width
      )

      # Convert empty strings to NULL (they cause issues like "Unknown colour name")
      params <- lapply(params, function(x) {
        if (is.character(x) && length(x) == 1 && x == "") NULL else x
      })

      # Remove parameters from the list if they are not known by the geom
      known_params <- switch(
        selected_geom(),
        "text" = c(known_aes()),
        "label" = c(
          known_aes,
          "label.padding",
          "label.r",
          "label.size"
        ),
        "curve" = c(
          known_aes,
          "curvature",
          "angle",
          "arrow",
          "arrow.fill",
          "lineend"
        ),
        "rect" = c(known_aes()),
        "textbox" = c(
          known_aes(),
          "fill",
          "box.padding",
          "width",
          "hjust"
        )
      )
      params <- params[names(params) %in% known_params]

      purrr::compact(params)
    })

    # Create list of aesthetics based on user input ----
    aes_list <- reactive({
      req(user_input)
      req(input$geom)
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
      req(this_layer())
      safely_combine_layers(all_layers)$result
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
        selected_geom() ==
          "text" ~ "Click where you want to place your annotation",
        selected_geom() == "label" ~ "Click where you want to place your label",
        selected_geom() ==
          "curve" ~ "Click where you want your line to begin and double-click where it should end",
        selected_geom() ==
          "rect" ~ "Click and drag to draw and adjust the rectangle, then click once anywhere else to set it",
        selected_geom() ==
          "textbox" ~ "Click where you want to place your textbox",
        TRUE ~ "No instruction defined for geom"
      )
    })

    output$plot <- renderPlot({
      plot +
        purrr::map(annot_calls(), eval)
    })

    output$rendered_plot <- renderUI({
      size_units <- input$size_units

      plot_width <- paste0(input$plot_width, size_units)
      plot_height <- paste0(input$plot_height, size_units)

      plotOutput(
        "plot",
        click = "plot_click",
        dblclick = "plot_dblclick",
        brush = shiny::brushOpts(id = "plot_brush"),
        width = plot_width,
        height = plot_height
      )
    })

    output$geom_opts <- renderUI({
      req(selected_geom())
      switch(
        selected_geom(),
        "text" = text_ui,
        "label" = label_ui,
        "curve" = curve_ui,
        "rect" = rect_ui,
        "textbox" = textbox_ui
      )
    })

    observeEvent(input$copy_button, {
      callstring <- calls_to_string(annot_calls())

      # Try to copy to clipboard, with fallback for systems without clipboard support
      clipboard_success <- tryCatch(
        {
          clipr::write_clip(callstring, object_type = "character")
          TRUE
        },
        error = function(e) {
          FALSE
        }
      )

      if (!clipboard_success) {
        message(
          "Could not copy to clipboard. ",
          "On Wayland, install 'wl-clipboard'. On X11, install 'xclip' or 'xsel'.\n",
          "Code printed below:\n\n",
          callstring,
          "\n"
        )
      }

      ggplot2::set_last_plot(plot)
      stopApp()
    })

    output$code_output <- renderPrint({
      if (length(annot_calls()) > 0) {
        annot_calls()
      }
    })
  }

  ggann_app <- shiny::shinyApp(ggann_ui, ggann_server)

  shiny::runGadget(
    app = ggann_app,
    viewer = shiny::dialogViewer(
      "Annotate plot with ggannotate",
      width = 1300,
      height = 780
    ),
    stopOnCancel = TRUE
  )
}
