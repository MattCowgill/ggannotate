library(tidyverse)


create_annot <- function(id, user_input) {
  moduleServer(id, function(input, output, session) {
    reactive({

      if (!is.null(user_input)) {
      layers <- names(user_input)

      df <- tibble(x = map_dbl(layers, .f = function(x) user_input[[x]]$x),
                   y = map_dbl(layers, .f = function(x) user_input[[x]]$y))

      geoms <- map_chr(layers, .f = function(x) user_input[[x]]$geom)
      geom <- geoms[!is.na(geoms)][1]
      geom <- ifelse(is.na(geom), "text", geom)

      ggplot2::layer(geom,
                     stat = "identity", position = "identity",
                     data = df,
                     mapping = aes(x = x, y = y),
                     params = list(label = "a label"))

    }
    })


  })
}

ui <- fluidPage(
  selectInput('layer', "Layer", choices = c(1:10)),
  selectInput('geom', 'Geom', choices = c("text", "point"), selected = "text"),
  plotOutput('plot', click = "plot_click", dblclick = "plot_dblclick", brush = "plot_brush"),
  actionButton("stop_button", "Stop app"),
  verbatimTextOutput('textout')
)

server <- function(input, output, session) {

  user_input <- reactiveValues()

  selected_layer <- reactive(input$layer)

  output$textout <- reactive(input$geom)

  observeEvent(input$plot_click, {
    layer <- as.character(selected_layer())

    user_input[[layer]]$x <- input$plot_click$x
    user_input[[layer]]$y <- input$plot_click$y
    user_input[[layer]]$geom <- input$geom
  })

  annot_call <- create_annot("annot", user_input)

  output$plot <- renderPlot({
    ggplot(mtcars, aes(x = wt, y = mpg)) +
      geom_point() +
      annot_call()
  })

  observe({
    if (input$stop_button > 0) {
      stopApp(input$plot_click)
    }
  })

}

#shinyApp(ui=ui, server=server)

runApp(list(ui = ui, server = server))
