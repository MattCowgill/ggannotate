base_text_ui <- tagList(

  #sidebarPanel(width = 10,
  fluidRow(column(12,
                  textInput("annotation", "Annotation", value = "My annotation",
                            width = "100%"))),
  fluidRow(column(4,
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

text_ui <- c(base_text_ui,
             tagList(
               fluidRow(column(4,
                               numericInput("angle", "Angle", value = 0, min = -360, max = 360,
                                            step = 1))
               )
             )
)

label_ui <- c(base_text_ui,
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
           sliderInput("angle", "Curve angle", value = 90, min = 0, max = 180,
                       step = 1,
                       ticks = FALSE))),
  fluidRow(
    column(6,
           sliderInput("arrow_length", "Arrow length (in)",
                       value = 0.1, min = 0, max = 1, step = 0.05, ticks = FALSE)),
    column(6,
           sliderInput("arrow_angle", "Arrowhead angle",
                       min = 0, max = 90, value = 30, step = 1, ticks = FALSE))
  )
)
