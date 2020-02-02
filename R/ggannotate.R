#' ggannotate
#' @name ggannotate
#' @export
#' @import shiny
#' @import ggplot2
#'

ggannotate <- function(plot_code = "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()") {

  ggann_ui <- basicPage(
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
  shiny::runGadget(app, viewer = shiny::dialogViewer("Annotate plot"))
}


