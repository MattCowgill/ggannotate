ggannPanel <- function(...) {
  shiny::wellPanel(
    style = "background: #f8fafc; border: 1px solid #e2e8f0; border-radius: 6px;",
    ...
  )
}

# Shiny UI ------

ggann_ui <- shiny::fillPage(
  theme = bslib::bs_theme(
    version = 5,
    bg = "#ffffff",
    fg = "#1e293b",
    primary = "#0d9488",
    secondary = "#64748b",
    font_scale = 0.9,
    base_font = bslib::font_google("Inter", local = FALSE)
  ),
  tags$head(
    tags$style(HTML(
      "hr.ggann_blue {
        border-top: 1px solid #e2e8f0;
        margin: 0.3em;
      }
      .control-label {
        font-size: 12px;
        font-weight: 600;
        color: #64748b;
        letter-spacing: 0.01em;
      }
      pre#code_output {
        font-family: 'SFMono-Regular', Consolas, 'Liberation Mono', Menlo, monospace;
        font-size: 12px;
        background: #1e293b;
        border: none;
        border-radius: 6px;
        color: #e2e8f0;
        padding: 12px;
      }
      .btn-delete {
        transition: color 0.15s, border-color 0.15s;
      }
      .btn-delete:hover {
        color: #ef4444 !important;
        border-color: #ef4444 !important;
      }
      body {
        background: #ffffff;
      }
      .well {
        box-shadow: none;
      }
      .irs-single, .irs-from, .irs-to,
      .irs--shiny .irs-single,
      .irs--shiny .irs-from,
      .irs--shiny .irs-to {
        color: #fff !important;
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
  shiny::fillRow(
    flex = c(1, 2),
    ggannPanel(
      style = "background: #f1f5f9; border: 1px solid #e2e8f0; border-radius: 6px;",
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
            class = "btn-delete",
            style = "width: 100%; background: transparent; color: #0d9488; border: 1px solid #0d9488; border-radius: 4px; font-size: 12px;"
          )
        )
      ),
      hr(class = "ggann_blue"),
      br(),
      fluidRow(column(12, uiOutput("geom_opts")))
    ),
    shiny::column(
      width = 12,
      style = "padding-left: 20px; padding-right: 20px;",
      div(
        textOutput("instruction"),
        style = "font-weight:bold; line-height:1.6em; font-size:1em"
      ),
      plotOutput(
        "plot",
        click = "plot_click",
        dblclick = "plot_dblclick",
        brush = shiny::brushOpts(id = "plot_brush"),
        width = "16cm",
        height = "10cm"
      ),
      div(
        style = "padding: 8px 0; border-top: 1px solid #e2e8f0; margin-top: 8px;",
        fluidRow(
          column(
            4,
            numericInput(
              "plot_width",
              "Plot width",
              value = 16,
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
      div(
        style = "padding: 8px 0;",
        actionButton(
          "copy_button",
          "Copy & close",
          style = "background: #0d9488; color: #fff; border: none; border-radius: 6px; font-weight: 600; font-size: 13px; padding: 8px 24px; margin-bottom: 8px;"
        ),
        verbatimTextOutput("code_output", placeholder = TRUE)
      )
    )
  )
)
