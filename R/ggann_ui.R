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
  miniUI::miniTabstripPanel(
    miniUI::miniTabPanel(
      title = "Annotate", icon = shiny::icon("tag"),
      miniUI::miniContentPanel(
        shiny::fillRow(
          flex = c(1, 2),
          shiny::wellPanel(
            fluidRow(
              column(
                6,
                selectInput("layer", "Annotation",
                  choices = 1:10,
                  selected = 1,
                  multiple = FALSE
                )
              ),
              column(
                6,
                selectInput("geom_1", "Geom",
                  choices = c("text", "label", "curve", "rect"),
                  selected = "text"
                )
              )
            ),
            hr(class = "black"),
            fluidRow(column(12, uiOutput("geom_opts")))
          ),
          shiny::column(
            width = 12,
            div(textOutput("instruction"),
              style = "color:black; font-weight:bold; line-height:1.6em; font-size:1em"
            ),
            uiOutput("rendered_plot"),
            shiny::wellPanel(
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
            )
          )
        )
      )
    ),
    miniUI::miniTabPanel(
      title = "Get the code", icon = shiny::icon("code"),
      miniUI::miniContentPanel(
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
    )
  )
)
