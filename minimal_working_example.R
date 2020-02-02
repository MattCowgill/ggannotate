
library(shiny)
library(ggplot2)

ui <- basicPage(
  textInput("annotation", "Annotation", value = ""),
  textInput("user_plot_code", "Plot code", value = "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"),
  plotOutput("plot1", click = "plot_click"),
  verbatimTextOutput("info")
)

server <- function(input, output) {
  
  user_input <- reactiveValues()
  
  observeEvent(input$plot_click, {
    user_input$x <- input$plot_click$x
    user_input$y <- input$plot_click$y
    user_input$annotation <- input$annotation
  })
  
  base_plot_code <- reactive({
   rlang::parse_expr(input$user_plot_code) 
  })
  

  output$plot1 <- renderPlot({
    eval(base_plot_code()) +
      annotate("text", x = user_input$x, y = user_input$y,
               label = user_input$annotation)
  })
  
  # output$info <- renderText({
  #   paste0("x=", user_input$x, "\ny=", user_input$y, "\nannotation=", user_input$annotation)
  # })
  
  output$info <- shiny::renderPrint(base_plot_code())
}

shinyApp(ui, server)