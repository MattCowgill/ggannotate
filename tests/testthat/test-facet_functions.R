
# test_list is a full replica of a list that is returned as `input$plot_click`
# by Shiny, on clicking in a plot

test_list <- list(
  x = 4,
  y = 23,
  coords_css = list(
    x = 247,
    y = 76
  ),
  coords_img = list(
    x = 424,
    y = 129
  ),
  panelvar1 = 6,
  panelvar2 = 0,
  mapping = list(
    x = "wt",
    y = "mpg",
    panelvar1 = "cyl",
    panelvar2 = "am"
  ),
  domain = list(
    left = 1.32,
    right = 5.62,
    bottom = 9.22,
    top = 35.1
  ),
  range = list(
    left = 316,
    right = 555,
    bottom = 285,
    top = 44.8
  ),
  log = list(
    x = NULL,
    y = NULL
  )
)



test_that("plot_facets() returns expected output", {

  facets <- plot_facets(test_list)

  expect_is(facets, "list")

  expect_equal(facets$levels,
               list(panelvar1 = 6,
                    panelvar2 = 0))

  expect_equal(facets$vars,
               list(panelvar1 = "cyl",
                    panelvar2 = "am"))
})



