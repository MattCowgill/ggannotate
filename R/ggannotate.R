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
#' @importFrom dplyr case_when
#' @importFrom clipr write_clip
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
        div(textOutput("instruction"),
             style="color:black; font-weight:bold; line-height:2em; font-size:1.25em"),
        uiOutput("rendered_plot"),
        hr(class = "black"),
        fluidRow(
          column(3,
                 actionButton("copy_button", "Copy code")),
          column(9,
                 verbatimTextOutput("code_output"))

        )

      )
    ),

  )

  # Shiny server ------

  ggann_server <- function(input, output) {

    user_input <- reactiveValues()

    observeEvent(input$plot_click, {
      user_input$x <- input$plot_click$x
      user_input$y <- input$plot_click$y
    })

    observeEvent(input$plot_dblclick,{
      user_input$x_dbl <- input$plot_dblclick$x
      user_input$y_dbl <- input$plot_dblclick$y
    })

    base_plot_code <- reactive(rlang::parse_expr(plot_code))

    user_args <- reactive({

      annot_no_esc <- gsub("\\n", "\n", input$annotation, fixed = TRUE)

      text_args <- list(geom = input$geom_1,
                     x = user_input$x,
                     y = user_input$y,
                     label = annot_no_esc,
                     angle = input$angle,
                     lineheight = input$lineheight,
                     hjust = input$hjust,
                     vjust = input$vjust,
                     colour = input$colour,
                     family = input$font,
                     fontface = input$fontface)


      curve_args <- list(geom = input$geom_1,
                         x = user_input$x_dbl,
                         y = user_input$y_dbl,
                         xend = user_input$x,
                         yend = user_input$y,
                         curvature = input$curvature,
                         angle = input$curve_angle,
                         arrow = arrow(angle = input$arrow_angle,
                                       length = unit(0.1, "inches"),
                                       ends = "last",
                                       type = "closed"))

      if (input$geom_1 == "text") {
        args <- text_args
      } else if (input$geom_1 == "label") {
        args <- c(text_args,
                  list(label.padding = unit(input$label.padding, "lines"),
                       label.r = unit(input$label.r, "lines"),
                       label.size = input$label.size))
      } else if (input$geom_1 == "curve") {
        args <- curve_args
      }


      list("args" = args)

    })

    output$instruction <- renderText({
      dplyr::case_when(input$geom_1 == "text" ~ "Click where you want to place your annotation",
                       input$geom_1 == "label" ~ "Click where you want to place your label",
                       input$geom_1 == "curve" ~ "Click where want your line to begin and double-click where it should end",
                       TRUE ~ "No instruction defined for geom")
    })

    output$plot <- renderPlot({
      any_null <- ifelse(is.null(user_args()$args$x) |
                           is.null(user_args()$args$xend) |
                           is.null(user_args()$args$yend) |
                           is.null(user_args()$args$y),
                         TRUE,
                         FALSE)

      if(user_args()$args$geom == "curve" &
         isTRUE(any_null)) {
        eval(base_plot_code())

      } else {
        eval(base_plot_code()) +
          rlang::exec("annotate", !!!user_args()$args)
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

      text_ui <- tagList(

        #sidebarPanel(width = 10,
        fluidRow(column(12,
                        textInput("annotation", "Annotation", value = "My annotation",
                                  width = "100%"))),
        fluidRow(column(4,
                        numericInput("angle", "Angle", value = 0, min = -360, max = 360,
                                     step = 1)),
                 column(4,
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

      label_ui <- c(text_ui,
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
                 sliderInput("curve_angle", "Curve angle", value = 90, min = 0, max = 180,
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
      rlang::expr("annotate"(!!!user_args()$args))
    })

    observeEvent(input$copy_button, {
      clip_code <- rlang::expr_text(annot_code())
      clip_code <- gsub('\\\"', '"', clip_code)
      clipr::write_clip(clip_code, object_type = "character")
    })

    output$code_output <- renderPrint({
      annot_code()
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

