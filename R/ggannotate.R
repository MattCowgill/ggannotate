#' ggannotate
#' @name ggannotate
#'
#' @param plot_code Code to construct a ggplot2
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
#' @importFrom rlang expr exec enquo get_expr expr_deparse
#' @importFrom dplyr case_when if_else
#' @importFrom clipr write_clip
#' @importFrom stringr str_squish
#'

ggannotate <- function(plot_code) {

  if (!interactive()) {
    stop("`ggannotate` only works in interactive sessions.")
  }

  # Wrangle code input -------
  if (!missing(plot_code)) {
    plot_code <- rlang::enquo(plot_code)
    plot_code <- rlang::get_expr(plot_code)
    plot_code <- rlang::expr_deparse(plot_code)
  }

  if (missing(plot_code)) {
    if (isFALSE(rstudioapi::isAvailable())) {
      stop("ggannotate requires RStudio to see your selection.",
           " Supply `plot_code` instead.")
    }
    plot_code <- rstudio_selection()
  }

  plot_code <- rstudio_text_tidy(plot_code)
  plot_code <- escape_newlines(sub("\n$", "", enc2utf8(plot_code)))
  plot_code <- paste(plot_code, collapse = "")

  # Shiny UI ------

  ggann_ui <- fluidPage(

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
        width = 3,
        fluidRow(column(6,
                        selectInput("geom_1", "Geom",
                                    choices = c("text", "label", "curve"),
                                    selected = "text"))),
        hr(class = "black"),
        fluidRow(column(12, uiOutput("geom_opts"))),
        hr(class = "black"),
        fluidRow(column(4,
                        numericInput("plot_width", "Plot width", value = 22.16, min = 0, step = 1)),
                 column(4,
                        numericInput("plot_height", "Plot height", value = 14.5, min = 0, step = 1)),
                 column(4,
                        selectInput("size_units",
                                    "Units ",
                                    choices = c("cm", "mm", "in", "px"),
                                    selected = "cm"))),
        hr(class = "black")
      ),
      mainPanel(
        width = 9,
        div(textOutput("instruction"),
             style="color:black; font-weight:bold; line-height:2em; font-size:1.25em"),
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

    user_input <- reactiveValues()

    built_base_plot <- reactive({
      p <- eval(base_plot_code())
      p <- ggplot2::ggplot_build(p)
      p
    })

    flipped_coords <- reactive({
      p <- built_base_plot()
      ggplot2::summarise_coord(p)$flip
    })

    observeEvent(input$plot_click, {
      user_input$x <- input$plot_click$x
      user_input$y <- input$plot_click$y

      user_input$facet_var1 <- input$mapping$panelvar1
      user_input$facet_level1 <- input$panelvar1

      user_input$facet_var2 <- input$mapping$panelvar2
      user_input$facet_level2 <- input$panelvar2

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

      print(isTRUE(flipped_coords()))
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

    base_plot_code <- reactive(rlang::parse_expr(plot_code))

    # Check whether axes are dates
    axis_classes <- reactive({
      p <- built_base_plot()

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

      user_arrow <- if(input$geom_1 == "curve") {
        arrow(angle = input$arrow_angle,
              length = unit(0.1, "inches"),
              ends = "last",
              type = "closed")
      } else {
        NULL
      }

      params <- list(
        angle = input$angle,
        lineheight = input$lineheight,
        hjust = input$hjust,
        vjust = input$vjust,
        colour = input$colour,
        family = input$font,
        fontface = input$fontface,
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

      selected_geom <- if (geom == "text") {
        ggplot2::GeomText
      } else if (geom == "label") {
        ggplot2::GeomLabel
      } else if (geom == "curve") {
        ggplot2::GeomCurve
      }

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
        annotate_all_facets = FALSE,
        facet_var1 = user_input$facet_var1,
        facet_level1 = user_input$facet_level1,
        facet_var2 = user_input$facet_var2,
        facet_level2 = user_input$facet_level2,
        params = params_list
      )

      print(layer_call)

      layer_call
    })

    output$instruction <- renderText({
      dplyr::case_when(input$geom_1 == "text" ~ "Click where you want to place your annotation",
                       input$geom_1 == "label" ~ "Click where you want to place your label",
                       input$geom_1 == "curve" ~ "Click where want your line to begin and double-click where it should end",
                       TRUE ~ "No instruction defined for geom")
    })

    output$plot <- renderPlot({

      show_annot <- if (is.null(user_input$x)) {
        FALSE
      } else if (input$geom_1 == "curve") {
        if(is.null(user_input$x) |
           is.null(user_input$x_dbl) |
           is.null(user_input$y) |
           is.null(user_input$y_dbl)) {
          FALSE
        }
      } else {
        TRUE
      }

      if (isFALSE(show_annot)) {
        eval(base_plot_code())
        } else {
        eval(base_plot_code()) +
          eval(annot_call())
      }
    })

    output$rendered_plot <- renderUI({

      css_units <- input$size_units

      plot_width <- paste0(input$plot_width, css_units)
      plot_height <- paste0(input$plot_height, css_units)

      plotOutput("plot", click = "plot_click",
                 dblclick = "plot_dblclick",
                 width = plot_width,
                 height = plot_height)
    })

    output$geom_opts <- renderUI({
      geom <- input$geom_1
      show_arrow <- ifelse(is.null(input$show_arrow),
                           TRUE,
                           input$show_arrow)

      base_text_ui <- tagList(

        #sidebarPanel(width = 10,
        fluidRow(column(12,
                        textInput("annotation", "Annotation", value = "My annotation",
                                  width = "100%"))),
        fluidRow(column(4,
                        numericInput("lineheight", "Lineheight", value = 1,
                                     min = 0, step = 0.05)),
                 column(4,
                        textInput("colour", "colour", value = "black"))),
        fluidRow(column(6,
                        sliderInput("hjust", "hjust", value = 0.5,
                                    min = 0, max = 1, step = 0.05, ticks = FALSE)),
                 column(6,
                        sliderInput("vjust", "vjust", value = 0.5,
                                    min = 0, max = 1, step = 0.05, ticks = FALSE))),
        fluidRow(column(6,
                        textInput("font", "font", value = "sans")),
                 column(6,
                        selectInput("fontface", "fontface", selected = "plain",
                                    choices = c("plain", "bold", "italic", "bold.italic"))))

      )

      text_ui <- c(base_text_ui,
                   tagList(
        fluidRow(column(4,
                        numericInput("angle", "Angle", value = 0, min = -360, max = 360,
                                                  step = 1))
                   )
        )
      )

      label_ui <- c(base_text_ui,
                    tagList(
                      fluidRow(column(4,
                                      numericInput("label.padding", "Label padding",
                                                   value = 0.25, step = 0.01)),
                               column(4,
                                      numericInput("label.r", "Label radius",
                                                   value = 0.15, step = 0.01)),
                               column(4,
                                      numericInput("label.size", "Label size",
                                                   value = 0.25, step = 0.01)))

                    ))

      curve_ui <- tagList(
        fluidRow(
          column(6,
                 sliderInput("curvature", "Curvature",
                             min = -1, max = 1, value = 0.5, step = 0.01,
                             ticks = FALSE)),
          column(6,
                 sliderInput("angle", "Curve angle", value = 90, min = 0, max = 180,
                              step = 1,
                             ticks = FALSE))),
        fluidRow(
          column(6,
                 sliderInput("arrow_angle", "Arrowhead angle",
                             min = 0, max = 90, value = 30, step = 1, ticks = FALSE))
        )
      )



      if (geom == "text") {
        text_ui
      } else if (geom == "label") {
        label_ui
      } else if (geom == "curve") {
        curve_ui
      } else {
        stop()
      }


    })

    annot_code <- reactive({
      rlang::expr(annot_call())
    })

    observeEvent(input$copy_button, {
      clip_code <- rlang::expr_text(annot_call())
      #clip_code <- gsub('\\\"', '"', clip_code)
      clip_code <- stringr::str_squish(clip_code)
      #clip_code <- base::strwrap(clip_code, width = 80)

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
                                                height = 750),
                   stopOnCancel = TRUE)
}

# from reprex
rstudio_selection <- function(context = rstudio_context()) {
  text <- rstudioapi::primary_selection(context)[["text"]]
}

rstudio_context <- function() {
  rstudioapi::getSourceEditorContext()
}

rstudio_text_tidy <- function(x) {
  Encoding(x) <- "UTF-8"
  if (length(x) == 1) {
    ## rstudio_selection() returns catenated text
    x <- strsplit(x, "\n")[[1]]
  }

  n <- length(x)
  if (!grepl("\n$", x[[n]])) {
    x[[n]] <- newline(x[[n]])
  }
  x
}

newline <- function(x) {
  paste0(x, "\n")
}

escape_newlines <- function (x) {
  gsub("\n", "\\\\n", x, perl = TRUE)
}

