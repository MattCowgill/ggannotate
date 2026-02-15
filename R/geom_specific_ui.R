base_text_ui <- tagList(
  # sidebarPanel(width = 10,
  fluidRow(
    column(
      12,
      textInput(
        "annotation",
        "Annotation",
        value = "My annotation",
        width = "100%"
      )
    )
  ),
  fluidRow(
    column(
      6,
      numericInput(
        "lineheight",
        "Lineheight",
        value = 1.2,
        min = 0,
        step = 0.05
      )
    ),
    column(
      6,
      textInput("colour", "Colour", value = "black")
    )
  ),
  fluidRow(
    column(
      6,
      sliderInput(
        "hjust",
        "hjust",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.05,
        ticks = FALSE
      )
    ),
    column(
      6,
      sliderInput(
        "vjust",
        "vjust",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.05,
        ticks = FALSE
      )
    )
  ),
  fluidRow(
    column(
      6,
      textInput("font", "font", value = "")
    ),
    column(
      6,
      selectInput(
        "fontface",
        "fontface",
        selected = "plain",
        choices = c("plain", "bold", "italic", "bold.italic")
      )
    )
  )
)

text_ui <- c(
  base_text_ui,
  tagList(
    fluidRow(
      column(
        4,
        numericInput(
          "size",
          "Font size",
          value = 11,
          min = 0,
          max = 48,
          step = 0.5
        )
      ),
      column(
        4,
        numericInput(
          "angle",
          "Angle",
          value = 0,
          min = -360,
          max = 360,
          step = 1
        )
      )
    )
  )
)

label_ui <- c(
  base_text_ui,
  tagList(
    fluidRow(
      column(
        4,
        numericInput(
          "size",
          "Font size",
          value = 11,
          min = 0,
          max = 48,
          step = 0.5
        )
      )
    ),
    fluidRow(
      column(
        4,
        numericInput(
          "label.padding",
          "Label padding",
          value = 0.25,
          step = 0.025
        )
      ),
      column(
        4,
        numericInput("label.r", "Label radius", value = 0.15, step = 0.025)
      ),
      column(
        4,
        numericInput("label.size", "Label size", value = 0.25, step = 0.05)
      )
    )
  )
)

curve_ui <- tagList(
  fluidRow(
    column(
      6,
      sliderInput(
        "size",
        "Line size",
        min = 0.1,
        max = 20,
        value = 0.5,
        step = 0.05
      )
    )
  ),
  fluidRow(
    column(
      6,
      sliderInput(
        "curvature",
        "Curvature",
        min = -1,
        max = 1,
        value = 0.5,
        step = 0.005,
        ticks = FALSE
      )
    ),
    column(
      6,
      sliderInput(
        "angle",
        "Curve angle",
        value = 90,
        min = 0,
        max = 180,
        step = 1,
        ticks = FALSE
      )
    )
  ),
  fluidRow(
    column(
      6,
      sliderInput(
        "arrow_length",
        "Arrow length (in)",
        value = 0.1,
        min = 0,
        max = 1,
        step = 0.05,
        ticks = FALSE
      )
    ),
    column(
      6,
      sliderInput(
        "arrow_angle",
        "Arrowhead angle",
        min = 0,
        max = 90,
        value = 30,
        step = 1,
        ticks = FALSE
      )
    )
  )
)

rect_ui <- tagList(
  fluidRow(
    column(
      6,
      sliderInput(
        "alpha",
        "Fill opacity (alpha)",
        min = 0,
        max = 1,
        value = 0.25,
        step = 0.05
      )
    ),
    column(
      6,
      textInput("fill", "Fill colour", value = "grey55")
    )
  ),
  fluidRow(
    column(
      6,
      textInput("colour", "Border colour", value = "black")
    ),
    column(
      6,
      numericInput(
        "size",
        "Border size",
        value = 0,
        min = 0,
        max = 5,
        step = 0.5
      )
    )
  )
)

textbox_ui <- tagList(
  fluidRow(
    column(
      12,
      textInput(
        "annotation",
        "Annotation",
        value = "My annotation",
        width = "100%"
      )
    )
  ),
  fluidRow(
    column(
      6,
      numericInput(
        "lineheight",
        "Lineheight",
        value = 1.2,
        min = 0,
        step = 0.05
      )
    ),
    column(
      6,
      textInput("colour", "Colour", value = "black")
    )
  ),
  fluidRow(
    column(
      4,
      numericInput(
        "size",
        "Font size",
        value = 11,
        min = 0,
        max = 48,
        step = 0.5
      )
    )
  ),
  fluidRow(
    column(
      6,
      sliderInput(
        "hjust",
        "hjust",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.05,
        ticks = FALSE
      )
    ),
    column(
      6,
      sliderInput(
        "vjust",
        "vjust",
        value = 0.5,
        min = 0,
        max = 1,
        step = 0.05,
        ticks = FALSE
      )
    )
  ),
  fluidRow(
    column(
      6,
      textInput("font", "font", value = "")
    ),
    column(
      6,
      selectInput(
        "fontface",
        "fontface",
        selected = "plain",
        choices = c("plain", "bold", "italic", "bold.italic")
      )
    )
  ),
  fluidRow(
    column(
      4,
      textInput("fill", "Box fill", value = "white")
    ),
    column(
      4,
      numericInput(
        "box.padding",
        "Box padding",
        value = 5.5,
        min = 0,
        step = 0.5
      )
    ),
    column(
      4,
      numericInput("width", "Width (inches)", value = NA, min = 0, step = 0.1)
    )
  )
)
