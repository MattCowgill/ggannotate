ggannPanel <- function(...) {
  shiny::wellPanel(
    style = "background: rgba(22, 80, 129, 0.05); border-style: none",
    ...
  )
}

# Shiny UI ------

ggann_ui <- miniUI::miniPage(
  tags$head(
    tags$style(HTML(
      "hr.ggann_blue {
        border-top: 0.5px solid rgba(22, 80, 129, 1);
        margin: 0.3em;
        }
        body {
        color: rgba(22, 80, 129, 1);
        }"
    )),
    tags$script(HTML(
      "
      Shiny.addCustomMessageHandler('resize-plot', function(msg) {
        var el = document.getElementById('plot');
        if (el) {
          el.style.width = msg.width;
          el.style.height = msg.height;
          $(el).trigger('resize');
        }
      });
    "
    ))
  ),
  miniUI::miniContentPanel(
    shiny::fillRow(
      flex = c(1, 2),
      ggannPanel(
        style = "background: rgba(22, 80, 129, 0.1); border-style: none",
        fluidRow(
          column(
            6,
            selectInput(
              "annot_layer",
              "Annotation layer",
              choices = 1:10,
              selected = 1,
              multiple = FALSE
            )
          ),
          column(
            6,
            selectInput(
              "geom",
              "Geom",
              choices = if (requireNamespace("ggtext", quietly = TRUE)) {
                c("text", "label", "curve", "rect", "textbox")
              } else {
                c("text", "label", "curve", "rect")
              },
              selected = "text"
            )
          )
        ),
        fluidRow(
          column(
            12,
            actionButton(
              "delete_layer",
              "Delete layer",
              style = "width: 100%; background: rgba(180, 50, 50, 0.7); color: white;"
            )
          )
        ),
        hr(class = "ggann_blue"),
        br(),
        fluidRow(column(12, uiOutput("geom_opts")))
      ),
      shiny::column(
        width = 12,
        div(
          textOutput("instruction"),
          style = "font-weight:bold; line-height:1.6em; font-size:1em"
        ),
        plotOutput(
          "plot",
          click = "plot_click",
          dblclick = "plot_dblclick",
          brush = shiny::brushOpts(id = "plot_brush"),
          width = "18cm",
          height = "10cm"
        ),
        ggannPanel(
          fluidRow(
            column(
              4,
              numericInput(
                "plot_width",
                "Plot width",
                value = 18,
                min = 0,
                step = 1
              )
            ), # 22.16
            column(
              4,
              numericInput(
                "plot_height",
                "Plot height",
                value = 10,
                min = 0,
                step = 1
              )
            ), # 14.5
            column(
              4,
              selectInput(
                "size_units",
                "Units  ",
                choices = c("cm", "mm", "in", "px"),
                selected = "cm"
              )
            )
          ),
        ),
        ggannPanel(
          fluidRow(
            column(
              2,
              actionButton(
                "copy_button",
                HTML("<b>Copy &<br/>close</b>"),
                width = "100%",
                style = "height:55px; background: rgba(22, 80, 129, 0.66); color: rgba(255, 255, 255, 1)"
              )
            ),
            column(
              10,
              verbatimTextOutput("code_output", placeholder = TRUE)
            )
          )
        )
      )
    )
  )
)
