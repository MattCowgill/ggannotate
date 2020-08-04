#' ggannotate
#' @name ggannotate
#'
#' @param plot Either a ggplot2 object, or code to construct a ggplot2
#' object. If blank, your current selection in RStudio will be used.
#'
#' @examples
#'
#' \dontrun{
#' p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
#'        geom_point()
#'
#' ggannotate(p)
#' }
#'
#' @export
#' @import shiny
#' @import ggplot2
#' @importFrom rstudioapi getSourceEditorContext primary_selection
#' @importFrom rlang expr exec enquo get_expr expr_deparse parse_expr
#' @importFrom dplyr case_when if_else
#' @importFrom miniUI miniPage
#' @importFrom clipr write_clip
#'

ggannotate <- function(plot) {
  if (!interactive()) {
    stop("`ggannotate` only works in interactive sessions.")
  }

  # Wrangle selection -------
  if (missing(plot)) {
    if (isFALSE(rstudioapi::isAvailable())) {
      stop(
        "ggannotate requires RStudio to see your selection.",
        " Supply `plot` instead."
      )
    }

    if (is.null(rstudio_selection())) {
      stop("Please select some plot code before invoking ggannotate.")
    }

    selection <- rstudio_selection()
    plot <- selection_as_plot(selection)
  }

  built_base_plot <- ggplot2::ggplot_build(plot)

  if (inherits(built_base_plot$layout$coord, "CoordPolar")) {
    stop("ggannotate() does not work with polar coordinates.")
  }

  # Shiny server ------

  ggann_server <- function(input, output, session) {
    observeEvent(input$done, shiny::stopApp())

    user_input <- reactiveValues()

    # Check whether axes are flipped
    flipped_coords <- ggplot2::summarise_coord(built_base_plot)$flip

    # Check whether axes are dates
    axis_classes <- check_if_date(built_base_plot)

    observeEvent(input$plot_click, {
      facets <- plot_facets(input$plot_click)
      facets <- correct_facets(facets, built_base_plot)
      user_input$facet_vars <- facets$vars
      user_input$facet_levels <- facets$levels

      user_input$x <- input$plot_click$x
      user_input$y <- input$plot_click$y

      # Date scales
      if (isTRUE(axis_classes$x_date)) {
        user_input$x <- num_to_date(user_input$x)
      }
      if (isTRUE(axis_classes$y_date)) {
        user_input$y <- num_to_date(user_input$y)
      }

      # Flipped scales
      if (isTRUE(flipped_coords)) {
        temp_x <- user_input$x
        temp_y <- user_input$y
        user_input$x <- temp_y
        user_input$y <- temp_x
      }
    })

    observeEvent(input$plot_dblclick, {
      user_input$x_dbl <- input$plot_dblclick$x
      user_input$y_dbl <- input$plot_dblclick$y

      # Date scales
      if (isTRUE(axis_classes$x_date)) {
        user_input$x_dbl <- num_to_date(user_input$x_dbl)
      }
      if (isTRUE(axis_classes$y_date)) {
        user_input$y_dbl <- num_to_date(user_input$y_dbl)
      }

      # Flipped scales
      if (isTRUE(flipped_coords)) {
        temp_x_dbl <- user_input$x_dbl
        temp_y_dbl <- user_input$y_dbl
        user_input$x_dbl <- temp_y_dbl
        user_input$y_dbl <- temp_x_dbl
      }
    })

    observeEvent(input$plot_brush, {
      facets <- plot_facets(input$plot_brush)
      facets <- correct_facets(facets, built_base_plot)
      user_input$facet_vars <- facets$vars
      user_input$facet_levels <- facets$levels

      user_input$xmin <- input$plot_brush$xmin
      user_input$xmax <- input$plot_brush$xmax
      user_input$ymin <- input$plot_brush$ymin
      user_input$ymax <- input$plot_brush$ymax

      # Date scales
      if (isTRUE(axis_classes$x_date)) {
        user_input$xmin <- num_to_date(user_input$xmin)
        user_input$xmax <- num_to_date(user_input$xmax)
      }

      if (isTRUE(axis_classes$y_date)) {
        user_input$ymin <- num_to_date(user_input$ymin)
        user_input$ymax <- num_to_date(user_input$ymax)
      }

      # Flipped scales
      if (isTRUE(flipped_coords)) {
        temp_xmin <- user_input$xmin
        temp_xmax <- user_input$xmax
        temp_ymin <- user_input$ymin
        temp_ymax <- user_input$ymax
        user_input$xmin <- temp_ymin
        user_input$xmax <- temp_ymax
        user_input$ymin <- temp_xmin
        user_input$ymax <- temp_xmax
      }
    })


    params_list <- reactive({
      user_arrow <- safe_arrow(
        angle = input$arrow_angle,
        length = input$arrow_length,
        ends = "last",
        type = "closed"
      )

      user_label_padding <- safe_unit(input$label.padding, "lines")
      user_label_r <- safe_unit(input$label.r, "lines")

      size <- ifelse(input$geom_1 %in% c("text", "label"),
        input$size / ggplot2::.pt,
        input$size
      )

      fontface <- case_when(input$fontface == "plain" ~ 1,
                input$fontface == "bold" ~ 2,
                input$fontface == "italic" ~ 3,
                input$fontface == "bold.italic" ~ 4,
                TRUE ~ NA_real_)

      user_alpha <- ifelse(input$geom_1 == "rect", input$alpha, 1)

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

      purrr::compact(params)
    })

    annot_call <- reactive({
      annot <- input$annotation
      annot_no_esc <- gsub("\\n", "\n", annot, fixed = TRUE)

      params_list <- params_list()

      geom <- input$geom_1

      selected_geom <- switch(geom,
        "text"  = ggplot2::GeomText,
        "label" = ggplot2::GeomLabel,
        "curve" = ggplot2::GeomCurve,
        "rect" = ggplot2::GeomRect
      )

      known_aes <- selected_geom$aesthetics()

      # Remove parameters from the list if they are not known by the geom
      known_params <- switch(geom,
        "text" = c(known_aes),
        "label" = c(
          known_aes, "label.padding", "label.r",
          "label.size"
        ),
        "curve" = c(
          known_aes, "curvature", "angle",
          "arrow", "arrow.fill", "lineend"
        ),
        "rect" = c(known_aes),
      )
      params_list <- params_list[names(params_list) %in% known_params]

      # Set aes to NULL if they are not known by the geom
      x <- switch("x" %in% known_aes, user_input$x, NULL)
      y <- switch("y" %in% known_aes, user_input$y, NULL)
      xend <- switch("xend" %in% known_aes, user_input$x_dbl, NULL)
      yend <- switch("yend" %in% known_aes, user_input$y_dbl, NULL)
      xmin <- switch("xmin" %in% known_aes, user_input$xmin, NULL)
      xmax <- switch("xmax" %in% known_aes, user_input$xmax, NULL)
      ymin <- switch("ymin" %in% known_aes, user_input$ymin, NULL)
      ymax <- switch("ymax" %in% known_aes, user_input$ymax, NULL)
      label <- switch("label" %in% known_aes, annot_no_esc, NULL)

      # Create the layer call
      layer_call <- make_layer(
        geom = geom,
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        xmin = xmin,
        xmax = xmax,
        ymin = ymin,
        ymax = ymax,
        label = label,
        facet_vars = user_input$facet_vars,
        facet_levels = user_input$facet_levels,
        params = params_list
      )

      layer_call
    })

    output$instruction <- renderText({
      dplyr::case_when(
        input$geom_1 == "text" ~ "Click where you want to place your annotation",
        input$geom_1 == "label" ~ "Click where you want to place your label",
        input$geom_1 == "curve" ~ "Click where you want your line to begin and double-click where it should end",
        input$geom_1 == "rect" ~ "Click and drag to draw and adjust the rectangle, then click once anywhere else to set it",
        TRUE ~ "No instruction defined for geom"
      )
    })

    output$plot <- renderPlot({
      show_annot <- if (is.null(user_input$x)) {
        FALSE
      } else if (input$geom_1 == "curve") {
        if (is.null(user_input$x) |
          is.null(user_input$x_dbl)) {
          FALSE
        } else if (isTRUE(user_input$x == user_input$x_dbl) &
          isTRUE(user_input$y == user_input$y_dbl)) {
          FALSE
        }
      } else if (input$geom_1 == "rect") {
        if (is.null(user_input$xmin) | is.null(user_input$xmax)) {
          FALSE
        } else if (isTRUE(user_input$xmin == user_input$xmax) &
          isTRUE(user_input$ymin == user_input$ymax)) {
          FALSE
        }
      } else {
        TRUE
      }

      if (isFALSE(show_annot)) {
        built_base_plot$plot
      } else {
        built_base_plot$plot +
          eval(annot_call())
      }
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
      switch(input$geom_1,
        "text"   = text_ui,
        "label"  = label_ui,
        "curve"  = curve_ui,
        "rect"  = rect_ui
      )
    })

    observeEvent(input$copy_button, {
      callstring <- call_to_string(annot_call())
      clipr::write_clip(callstring, object_type = "character")
      ggplot2::set_last_plot(built_base_plot$plot)
      stopApp()
    })

    output$code_output <- renderPrint({
      annot_call()
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
