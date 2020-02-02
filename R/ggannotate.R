#' ggannotate
#' @name ggannotate
#' @export
#' @import shiny
#' @import ggplot2
#' @importFrom miniUI miniPage
#' @importFrom rstudioapi getSourceEditorContext primary_selection
#'

ggannotate <- function() {

  # code input

  plot_code <- rstudio_selection()
  plot_code <- escape_newlines(sub("\n$", "", enc2utf8(plot_code)))
  plot_code <- paste(plot_code, collapse = "")

  ggann_ui <- miniUI::miniPage(
    textInput("annotation", "Annotation", value = "My annotation"),
    textInput("user_plot_code", "Plot code", value = plot_code),
    numericInput("plot_width", "Plot width (pixels)", value = 800, min = 0, step = 1),
    numericInput("plot_height", "Plot height (pixels)", value = 400, min = 0, step = 1),
    uiOutput("plot"),
    verbatimTextOutput("info")
  )

  ggann_server <- function(input, output) {

    user_input <- reactiveValues()

    observeEvent(input$plot_click, {
      user_input$x <- input$plot_click$x
      user_input$y <- input$plot_click$y
      user_input$annotation <- input$annotation
    })

    base_plot_code <- reactive(rlang::parse_expr(input$user_plot_code))


    user_args <- reactive({

      args_1 <- list(geom = "text",
                     x = user_input$x,
                     y = user_input$y,
                     label = user_input$annotation)

      list("args_1" = args_1)

    })

    output$plot1 <- renderPlot({

      eval(base_plot_code()) +
        do.call("annotate", args = user_args()$args_1)
    })

    output$plot <- renderUI({
      plotOutput("plot1", click = "plot_click", width = input$plot_width)
    })

    output$info <- shiny::renderPrint(call("annotate", args = user_args()$args_1))
  }

  app <- shiny::shinyApp(ggann_ui, ggann_server)
  shiny::runGadget(app, viewer = shiny::dialogViewer("Annotate plot with ggannotate",
                                                     width = 1000,
                                                     height = 800))
}

# from reprex
rstudio_selection <- function(context = rstudio_context()) {
  text <- rstudioapi::primary_selection(context)[["text"]]
  rstudio_text_tidy(text)
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

