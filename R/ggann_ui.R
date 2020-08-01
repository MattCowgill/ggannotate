# Shiny UI ------
ggann_ui <- miniUI::miniPage(
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
      fluidRow(column(
        6,
        selectInput("geom_1", "Geom",
                    choices = c("text", "label", "curve", "rect"),
                    selected = "text"
        )
      )),
      hr(class = "black"),
      fluidRow(column(12, uiOutput("geom_opts"))),
      hr(class = "black"),
      fluidRow(
        column(
          4,
          numericInput("plot_width", "Plot width", value = 18, min = 0, step = 1)
        ), # 22.16
        column(
          4,
          numericInput("plot_height", "Plot height", value = 10, min = 0, step = 1)
        ), # 14.5
        column(
          4,
          selectInput("size_units",
                      "Units  ",
                      choices = c("cm", "mm", "in", "px"),
                      selected = "cm"
          )
        )
      ),
      hr(class = "black")
    ),
    mainPanel(
      width = 8,
      div(textOutput("instruction"),
          style = "color:black; font-weight:bold; line-height:1.6em; font-size:1em"
      ),
      uiOutput("rendered_plot"),
      hr(class = "black"),
      fluidRow(
        column(
          2,
          actionButton("copy_button", HTML("<b>Copy code<br/>and close</b>"),
                       width = "100%",
                       style = "height:55px; "
          )
        ),
        column(
          10,
          verbatimTextOutput("code_output")
        )
      )
    )
  ),
)
