#' Interactively annotate a ggplot2 plot
#'
#' Launches an interactive Shiny gadget that lets you point and click
#' on a ggplot2 plot to add annotations, then generates the code to
#' reproduce them.
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
#' @importFrom bslib bs_theme font_google
#' @importFrom clipr write_clip
#'

ggannotate <- function(plot = last_plot()) {
  if (!interactive()) {
    stop("`ggannotate` only works in interactive sessions.")
  }

  stopifnot(inherits(plot, "gg"))

  # Preserve last_plot() so the Shiny app's internal rendering doesn't

  # overwrite it
  saved_last_plot <- ggplot2::last_plot()
  on.exit(ggplot2::set_last_plot(saved_last_plot), add = TRUE)

  built_base_plot <- ggplot2::ggplot_build(plot)

  if (is_polar_coord(built_base_plot)) {
    stop("ggannotate() does not work with polar coordinates.")
  }

  # Shiny server ------

  # Work around Shiny < 1.11 bug where ggplot2 4.x S7 class dispatch
  # bypasses Shiny's local print.ggplot override, breaking coordmap
  # extraction. We pre-build the plot and return the gtable structure
  # directly, with a globally registered print method so S3 dispatch
  # finds it. See https://github.com/rstudio/shiny/issues/4226
  needs_coordmap_fix <- utils::packageVersion("ggplot2") >= "4.0.0" &&
    utils::packageVersion("shiny") < "1.11.0"
  if (needs_coordmap_fix) {
    print.ggplot_build_gtable <- function(x, ...) {
      grid::grid.newpage()
      grid::grid.draw(x$gtable)
      x
    }
    registerS3method(
      "print",
      "ggplot_build_gtable",
      print.ggplot_build_gtable,
      envir = asNamespace("base")
    )
  }

  ggann_server <- function(input, output, session) {
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

      # For curves, clear the end point so the user can reposition the start
      if (selected_geom() == "curve") {
        user_input$x_dbl <- NULL
        user_input$y_dbl <- NULL
      }
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
        ends = input$arrow_ends %||% "last",
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

      fontface <- input$fontface

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
          known_aes(),
          "label.padding",
          "label.r",
          "label.size"
        ),
        "curve" = c(
          known_aes(),
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
          "hjust",
          "vjust"
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

      # For rect, exclude x/y — rect requires xmin/xmax/ymin/ymax from brush
      if (selected_geom() == "rect") {
        aes[c("x", "y")] <- NULL
      }

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
      purrr::compact(list(
        geom = selected_geom(),
        aes = aes_list(),
        params = params_list(),
        facets = facets_list()
      ))
    })

    all_layers <- reactiveValues()
    active_layer <- reactiveVal(1L)
    layer_states <- reactiveValues()
    pending_restore <- reactiveVal(NULL)

    # Save current layer to all_layers explicitly (not via auto-observe)
    save_current_layer <- function() {
      all_layers[[as.character(active_layer())]] <- this_layer()
      layer_states[[as.character(active_layer())]] <- list(
        geom = input$geom,
        annotation = input$annotation,
        colour = input$colour,
        fill = input$fill,
        size = input$size,
        angle = input$angle,
        lineheight = input$lineheight,
        hjust = input$hjust,
        vjust = input$vjust,
        font = input$font,
        fontface = input$fontface,
        curvature = input$curvature,
        arrow_length = input$arrow_length,
        arrow_angle = input$arrow_angle,
        arrow_ends = input$arrow_ends,
        alpha = input$alpha,
        label.padding = input$`label.padding`,
        label.r = input$`label.r`,
        label.size = input$`label.size`,
        box.padding = input$`box.padding`,
        width = input$width,
        x = user_input$x,
        y = user_input$y,
        x_dbl = user_input$x_dbl,
        y_dbl = user_input$y_dbl,
        xmin = user_input$xmin,
        xmax = user_input$xmax,
        ymin = user_input$ymin,
        ymax = user_input$ymax,
        facet_vars = user_input$facet_vars,
        facet_levels = user_input$facet_levels
      )
    }

    update_layer_labels <- function() {
      stored <- reactiveValuesToList(all_layers)

      # Include the live current layer if it has content (aes set)
      current <- tryCatch(this_layer(), error = function(e) NULL)
      current_key <- as.character(active_layer())
      if (!is.null(current) && length(current$aes) > 0) {
        stored[[current_key]] <- current
      }

      existing_keys <- names(stored)
      existing_nums <- as.integer(existing_keys)

      if (length(existing_nums) == 0) {
        next_num <- 1L
      } else {
        next_num <- max(existing_nums) + 1L
      }

      all_choices <- c(existing_keys, as.character(next_num))
      labels <- all_choices
      for (i in seq_along(existing_keys)) {
        key <- existing_keys[[i]]
        labels[[i]] <- paste0(key, " <", stored[[key]]$geom, ">")
      }
      names(all_choices) <- labels

      updateSelectInput(
        session,
        "annot_layer",
        choices = all_choices,
        selected = as.character(active_layer())
      )
    }

    # Update dropdown whenever the current layer's content changes
    observe({
      this_layer()
      update_layer_labels()
    })

    # Handle layer switching
    observeEvent(input$annot_layer, {
      new_layer <- as.integer(input$annot_layer)
      old_layer <- active_layer()
      if (new_layer == old_layer) {
        return()
      }

      # Save current layer before switching
      save_current_layer()

      active_layer(new_layer)
      update_layer_labels()

      # Restore stored state for new layer
      stored <- layer_states[[as.character(new_layer)]]
      if (!is.null(stored)) {
        # Restore coordinates
        user_input$x <- stored$x
        user_input$y <- stored$y
        user_input$x_dbl <- stored$x_dbl
        user_input$y_dbl <- stored$y_dbl
        user_input$xmin <- stored$xmin
        user_input$xmax <- stored$xmax
        user_input$ymin <- stored$ymin
        user_input$ymax <- stored$ymax
        user_input$facet_vars <- stored$facet_vars
        user_input$facet_levels <- stored$facet_levels

        # Restore geom selection
        updateSelectInput(session, "geom", selected = stored$geom)

        # Queue styling restoration for after UI renders
        pending_restore(stored)
      } else {
        # Clear state for empty layer
        user_input$x <- NULL
        user_input$y <- NULL
        user_input$x_dbl <- NULL
        user_input$y_dbl <- NULL
        user_input$xmin <- NULL
        user_input$xmax <- NULL
        user_input$ymin <- NULL
        user_input$ymax <- NULL
        user_input$facet_vars <- NULL
        user_input$facet_levels <- NULL
      }
    })

    # Apply pending restore after geom-specific UI has rendered
    observe({
      state <- pending_restore()
      req(state)
      req(input$geom == state$geom)
      # Wait for UI to be fully rendered before updating inputs
      session$onFlushed(
        function() {
          session$onFlushed(
            function() {
              if (!is.null(state$annotation)) {
                updateTextInput(session, "annotation", value = state$annotation)
              }
              if (!is.null(state$colour)) {
                colourpicker::updateColourInput(
                  session,
                  "colour",
                  value = state$colour
                )
              }
              if (!is.null(state$fill)) {
                colourpicker::updateColourInput(
                  session,
                  "fill",
                  value = state$fill
                )
              }
              if (!is.null(state$size)) {
                updateNumericInput(session, "size", value = state$size)
              }
              if (!is.null(state$lineheight)) {
                updateNumericInput(
                  session,
                  "lineheight",
                  value = state$lineheight
                )
              }
              if (!is.null(state$hjust)) {
                updateSliderInput(session, "hjust", value = state$hjust)
              }
              if (!is.null(state$vjust)) {
                updateSliderInput(session, "vjust", value = state$vjust)
              }
              if (!is.null(state$font)) {
                updateTextInput(session, "font", value = state$font)
              }
              if (!is.null(state$fontface)) {
                updateSelectInput(
                  session,
                  "fontface",
                  selected = state$fontface
                )
              }
              if (!is.null(state$angle)) {
                updateNumericInput(session, "angle", value = state$angle)
              }
              if (!is.null(state$curvature)) {
                updateSliderInput(session, "curvature", value = state$curvature)
              }
              if (!is.null(state$arrow_length)) {
                updateSliderInput(
                  session,
                  "arrow_length",
                  value = state$arrow_length
                )
              }
              if (!is.null(state$arrow_angle)) {
                updateSliderInput(
                  session,
                  "arrow_angle",
                  value = state$arrow_angle
                )
              }
              if (!is.null(state$arrow_ends)) {
                updateSelectInput(
                  session,
                  "arrow_ends",
                  selected = state$arrow_ends
                )
              }
              if (!is.null(state$alpha)) {
                updateSliderInput(session, "alpha", value = state$alpha)
              }
              if (!is.null(state$`label.padding`)) {
                updateNumericInput(
                  session,
                  "label.padding",
                  value = state$`label.padding`
                )
              }
              if (!is.null(state$`label.r`)) {
                updateNumericInput(session, "label.r", value = state$`label.r`)
              }
              if (!is.null(state$`label.size`)) {
                updateNumericInput(
                  session,
                  "label.size",
                  value = state$`label.size`
                )
              }
              if (!is.null(state$`box.padding`)) {
                updateNumericInput(
                  session,
                  "box.padding",
                  value = state$`box.padding`
                )
              }
              if (!is.null(state$width)) {
                updateNumericInput(session, "width", value = state$width)
              }
              isolate(pending_restore(NULL))
            },
            once = TRUE
          )
        },
        once = TRUE
      )
    })

    # Handle layer deletion
    observeEvent(input$delete_layer, {
      current <- as.character(active_layer())
      all_layers[[current]] <- NULL
      layer_states[[current]] <- NULL

      # Clear current UI state
      user_input$x <- NULL
      user_input$y <- NULL
      user_input$x_dbl <- NULL
      user_input$y_dbl <- NULL
      user_input$xmin <- NULL
      user_input$xmax <- NULL
      user_input$ymin <- NULL
      user_input$ymax <- NULL
      user_input$facet_vars <- NULL
      user_input$facet_levels <- NULL

      update_layer_labels()
    })

    combined_layers <- reactive({
      # Use tryCatch so that a transient error in this_layer() (e.g. during
      # geom UI transition when inputs are momentarily NULL) doesn't hide
      # previously-saved layers
      current <- tryCatch(this_layer(), error = function(e) NULL)
      layers <- reactiveValuesToList(all_layers)
      if (!is.null(current)) {
        layers[[as.character(active_layer())]] <- current
      }
      layers <- purrr::compact(layers)
      # Filter out layers missing required aesthetics before combining,
      # otherwise an incomplete layer (e.g. no x/y yet) can merge with a
      # valid one and introduce NA rows
      layers <- Filter(
        function(l) {
          layer_obj <- tryCatch(
            eval(make_layer(
              geom = l$geom,
              aes = l$aes,
              params = l$params,
              facets = l$facets
            )),
            error = function(e) NULL
          )
          !is.null(layer_obj) && has_req_aes(layer_obj)
        },
        layers
      )
      if (length(layers) == 0) {
        return(NULL)
      }
      safely_combine_layers(layers)$result
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
      curve_has_start <- selected_geom() == "curve" &&
        !is.null(user_input$x) &&
        is.null(user_input$x_dbl)

      dplyr::case_when(
        selected_geom() ==
          "text" ~ "Click where you want to place your annotation",
        selected_geom() == "label" ~ "Click where you want to place your label",
        curve_has_start ~ "Now double-click where the line should end",
        selected_geom() == "curve" ~ "Click where you want your line to begin",
        selected_geom() ==
          "rect" ~ "Click and drag to draw and adjust the rectangle, then click once anywhere else to set it",
        selected_geom() ==
          "textbox" ~ "Click where you want to place your textbox",
        TRUE ~ "No instruction defined for geom"
      )
    })

    curve_start_marker <- reactive({
      if (
        selected_geom() == "curve" &&
          !is.null(user_input$x) &&
          !is.null(user_input$y) &&
          is.null(user_input$x_dbl)
      ) {
        ggplot2::annotate(
          "point",
          x = user_input$x,
          y = user_input$y,
          colour = "#0d9488",
          size = 3,
          shape = 4,
          stroke = 1.5
        )
      }
    })

    output$plot <- renderPlot({
      p <- plot + purrr::map(annot_calls(), eval) + curve_start_marker()
      if (needs_coordmap_fix) {
        build <- ggplot2::ggplot_build(p)
        gtable <- ggplot2::ggplot_gtable(build)
        structure(
          list(build = build, gtable = gtable),
          class = "ggplot_build_gtable"
        )
      } else {
        p
      }
    })

    observe({
      w <- paste0(input$plot_width, input$size_units)
      h <- paste0(input$plot_height, input$size_units)
      session$sendCustomMessage("resize-plot", list(width = w, height = h))
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
      save_current_layer()
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
