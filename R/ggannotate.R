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
#' @importFrom miniUI miniPage
#' @importFrom rstudioapi getSourceEditorContext primary_selection
#' @importFrom rlang expr exec enquo get_expr expr_deparse
#'

ggannotate <- function(plot_code) {

  # code input

  if (!interactive()) {
    stop("`ggannotate` only works in interactive sessions.")
  }

  if (!missing(plot_code)) {
    plot_code <- rlang::enquo(plot_code)
    plot_code <- rlang::get_expr(plot_code)
    plot_code <- rlang::expr_deparse(plot_code)
  }

  if (missing(plot_code)) {
    plot_code <- rstudio_selection()
  }
  plot_code <- rstudio_text_tidy(plot_code)
  plot_code <- escape_newlines(sub("\n$", "", enc2utf8(plot_code)))
  plot_code <- paste(plot_code, collapse = "")

  ggann_ui <- fluidPage(
    textInput("annotation", "Annotation", value = "My annotation"),
    textInput("user_plot_code", "Plot code", value = plot_code),
    numericInput("plot_width", "Plot width (pixels)", value = 800, min = 0, step = 1),
    numericInput("plot_height", "Plot height (pixels)", value = 400, min = 0, step = 1),
    uiOutput("rendered_plot"),
    verbatimTextOutput("code_output")
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

      annot_no_esc <- gsub("\\n", "\n", user_input$annotation, fixed = TRUE)

      args_1 <- list(geom = "text",
                     x = user_input$x,
                     y = user_input$y,
                     label = annot_no_esc)

      list("args_1" = args_1)

    })



    output$plot <- renderPlot({
      eval(base_plot_code()) +
        #do.call("annotate", args = user_args()$args_1)
        rlang::exec("annotate", !!!user_args()$args_1)
    })

    output$rendered_plot <- renderUI({
      plotOutput("plot", click = "plot_click", width = input$plot_width)
    })


    output$code_output <- renderPrint({
      rlang::expr("annotate"(!!!user_args()$args_1))
    })

  }

  ggann_app <- shiny::shinyApp(ggann_ui, ggann_server)
  shiny::runGadget(app = ggann_app,
                   viewer = shiny::dialogViewer("Annotate plot with ggannotate",
                                                width = 1000,
                                                height = 800))
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

