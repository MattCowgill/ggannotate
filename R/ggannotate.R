#' ggannotate
#' @name ggannotate
#'
#' @param plot Either a ggplot2 object, or code to construct a ggplot2
#' object. If blank, your current selection in RStudio will be used.
#'
#' @examples
#'
#'\donttest{
#' ggannotate("ggplot(mtcars, aes(x = wt, y = mpg)) +
#'     geom_point(col = 'orange')")
#'}
#'
#' @export
#' @import shiny
#' @import ggplot2
#' @importFrom rstudioapi getSourceEditorContext primary_selection
#' @importFrom rlang expr exec enquo get_expr expr_deparse parse_expr
#' @importFrom dplyr case_when if_else
#' @importFrom miniUI miniPage
#' @importFrom clipr write_clip
#' @importFrom stringr str_replace_all str_squish
#'

ggannotate <- function(plot) {

  if (!interactive()) {
    stop("`ggannotate` only works in interactive sessions.")
  }

  # Wrangle selection -------
  if (missing(plot)) {
    if (isFALSE(rstudioapi::isAvailable())) {
      stop("ggannotate requires RStudio to see your selection.",
           " Supply `plot` instead.")
    }

    if (is.null(rstudio_selection())) {
      stop("Please select some plot code before invoking ggannotate.")
    }

    plot <- rstudio_selection()
    plot <- selection_as_plot(plot)
  }

  built_base_plot <- ggplot2::ggplot_build(plot)

  # Shiny UI ------
  ggann_ui <- miniUI::miniPage(

    miniUI::gadgetTitleBar(title = "Annotate your plot",
                           left = NULL,
                           right = miniUI::miniTitleBarButton("done", "Done", primary = TRUE)),

    tags$head(
      tags$style(HTML(
        "hr.black {
        border: 0.4px solid #6a737b;
        margin: 0.2em;
        }"
        ))
      ),

    sidebarLayout(
      sidebarPanel(
        width = 4,
        fluidRow(column(6,
                        selectInput("geom_1", "Geom",
                                    choices = c("text", "label", "curve"),
                                    selected = "text"))),
        hr(class = "black"),
        fluidRow(column(12, uiOutput("geom_opts"))),
        hr(class = "black"),
        fluidRow(column(4,
                        numericInput("plot_width", "Plot width", value = 18, min = 0, step = 1)), #22.16
                 column(4,
                        numericInput("plot_height", "Plot height", value = 10, min = 0, step = 1)), #14.5
                 column(4,
                        selectInput("size_units",
                                    "Units  ",
                                    choices = c("cm", "mm", "in", "px"),
                                    selected = "cm"))),
        hr(class = "black")
      ),
      mainPanel(
        width = 8,
        div(textOutput("instruction"),
             style="color:black; font-weight:bold; line-height:1.6em; font-size:1em"),
        uiOutput("rendered_plot"),
        hr(class = "black"),
        fluidRow(
          column(2,
                 actionButton("copy_button", "Copy code", width = "100%")),
          column(10,
                 verbatimTextOutput("code_output"))

        )

      )
    ),

  )

  # Shiny server ------

  ggann_server <- function(input, output) {

    observeEvent(input$done, shiny::stopApp())

    user_input <- reactiveValues()

    flipped_coords <- reactive({
      ggplot2::summarise_coord(built_base_plot)$flip
    })

    observeEvent(input$plot_click, {
      user_input$x <- input$plot_click$x
      user_input$y <- input$plot_click$y

      user_input$facet_var1 <- input$plot_click$mapping$panelvar1
      user_input$facet_level1 <- input$plot_click$panelvar1

      user_input$facet_var2 <- input$plot_click$mapping$panelvar2
      user_input$facet_level2 <- input$plot_click$panelvar2

      user_input$facet_var3 <- input$plot_click$mapping$panelvar3
      user_input$facet_level3 <- input$plot_click$panelvar3

      user_input$facet_var4 <- input$plot_click$mapping$panelvar4
      user_input$facet_level4 <- input$plot_click$panelvar4

      user_input$facet_vars <- list(user_input$facet_var1,
                                    user_input$facet_var2,
                                    user_input$facet_var3,
                                    user_input$facet_var4)

      user_input$facet_levels <- list(user_input$facet_level1,
                                      user_input$facet_level2,
                                      user_input$facet_level3,
                                      user_input$facet_level4)

      # Date scales
      if (isTRUE(axis_classes()$x_date)) {
        user_input$x <- as.Date(user_input$x, origin = "1970-01-01")
      }
      if (isTRUE(axis_classes()$y_date)) {
        user_input$y <- as.Date(user_input$y, origin = "1970-01-01")
      }

      # Flipped scales
      if (isTRUE(flipped_coords())) {
        temp_x <- user_input$x
        temp_y <- user_input$y
        user_input$x <- temp_y
        user_input$y <- temp_x
      }

    })

    observeEvent(input$plot_dblclick,{
      user_input$x_dbl <- input$plot_dblclick$x
      user_input$y_dbl <- input$plot_dblclick$y

      # Date scales
      if (isTRUE(axis_classes()$x_date)) {
        user_input$x_dbl <- as.Date(user_input$x_dbl, origin = "1970-01-01")
      }
      if (isTRUE(axis_classes()$y_date)) {
        user_input$y_dbl <- as.Date(user_input$y_dbl, origin = "1970-01-01")
      }

      # Flipped scales
      if (isTRUE(flipped_coords())) {
        temp_x_dbl <- user_input$x_dbl
        temp_y_dbl <- user_input$y_dbl
        user_input$x_dbl <- temp_y_dbl
        user_input$y_dbl <- temp_x_dbl
      }

    })


    # Check whether axes are dates
    axis_classes <- reactive({
      p <- built_base_plot

      x_scale <- p$layout$panel_scales_x[[1]]
      y_scale <- p$layout$panel_scales_y[[1]]

      x_date <- dplyr::if_else(inherits(x_scale, "ScaleContinuousDate"),
                               TRUE,
                               FALSE)

      y_date <- dplyr::if_else(inherits(y_scale, "ScaleContinuousDate"),
                               TRUE,
                               FALSE)

      list("x_date" = x_date,
           "y_date" = y_date)
    })

    params_list <- reactive({

      user_arrow <- safe_arrow(angle = input$arrow_angle,
                               length = input$arrow_length,
                               ends = "last",
                               type = "closed")

      user_label_padding <- safe_unit(input$label.padding, "lines")
      user_label_r <- safe_unit(input$label.r, "lines")

      size <- ifelse(input$geom_1 %in% c("text", "label"),
                     input$size / ggplot2::.pt,
                     input$size)

      params <- list(
        size = size,
        angle = input$angle,
        lineheight = input$lineheight,
        hjust = input$hjust,
        vjust = input$vjust,
        colour = input$colour,
        family = input$font,
        fontface = input$fontface,
        label.padding = user_label_padding,
        label.size = input$label.size,
        label.r = user_label_r,
        curvature = input$curvature,
        arrow = user_arrow
      )

      purrr::compact(params)

    })

    annot_call <- reactive({

      annot <- input$annotation

      annot_no_esc <- gsub("\\n", "\n", annot, fixed = TRUE)

      params_list <- params_list()

      geom <- input$geom_1

      selected_geom <- switch (geom,
        "text"  = ggplot2::GeomText,
        "label" = ggplot2::GeomLabel,
        "curve" = ggplot2::GeomCurve
      )

      known_aes <- selected_geom$aesthetics()

      # Remove parameters from the list if they are not known by the geom
      known_params <- switch (geom,
        "text" = c(known_aes),
        "label" = c(known_aes, "label.padding", "label.r",
                        "label.size"),
        "curve" = c(known_aes, "curvature", "angle",
                          "arrow", "arrow.fill", "lineend")
      )
      params_list <- params_list[names(params_list) %in% known_params]

      # Set aes to NULL if they are not known by the geom
      x <- switch("x" %in% known_aes, user_input$x, NULL)
      y <- switch("y" %in% known_aes, user_input$y, NULL)
      xend <- switch("xend" %in% known_aes, user_input$x_dbl, NULL)
      yend <- switch("yend" %in% known_aes, user_input$y_dbl, NULL)
      label <- switch("label" %in% known_aes, annot_no_esc, NULL)

      # Create the layer call
      layer_call <- make_layer(
        geom = geom,
        x = x,
        y = y,
        xend = xend,
        yend = yend,
        label = label,
        facet_vars = user_input$facet_vars,
        facet_levels = user_input$facet_levels,
        params = params_list
      )

      layer_call
    })

    output$instruction <- renderText({
      dplyr::case_when(input$geom_1 == "text" ~ "Click where you want to place your annotation",
                       input$geom_1 == "label" ~ "Click where you want to place your label",
                       input$geom_1 == "curve" ~ "Click where you want your line to begin and double-click where it should end",
                       TRUE ~ "No instruction defined for geom")
    })

    output$plot <- renderPlot({

      show_annot <- if (is.null(user_input$x)) {
        FALSE
      } else if (input$geom_1 == "curve") {
        if(is.null(user_input$x) |
           is.null(user_input$x_dbl) ) {
          FALSE
        } else if (isTRUE(user_input$x == user_input$x_dbl) &
                   isTRUE(user_input$y == user_input$y_dbl)) {
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
                 width = plot_width,
                 height = plot_height)
    })

    output$geom_opts <- renderUI({
      switch (input$geom_1,
        "text"   = text_ui,
        "label"  = label_ui,
        "curve"  = curve_ui
      )
    })

    observeEvent(input$copy_button, {
      clip_code <- rlang::expr_text(annot_call())
      clip_code <- stringr::str_squish(clip_code)
      clip_code <- stringr::str_replace_all(clip_code, ", ", ",\n")
      clipr::write_clip(clip_code, object_type = "character")
    })

    output$code_output <- renderPrint({
      annot_call()
    })

  }

  ggann_app <- shiny::shinyApp(ggann_ui, ggann_server)

  shiny::runGadget(app = ggann_app,
                   viewer = shiny::dialogViewer("Annotate plot with ggannotate",
                                                width = 1300,
                                                height = 780),
                   stopOnCancel = TRUE)
}

