
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
    panelvar1 = "carb",
    panelvar2 = "cyl"
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
               list(panelvar1 = "carb",
                    panelvar2 = "cyl"))
})


base_plot <- p <- mtcars %>%
  ggplot(aes(x = wt, y = mpg)) +
  geom_point() +
  facet_wrap(carb~factor(cyl))

built_base_plot <- ggplot2::ggplot_build(base_plot)

test_that("facet vars undisturbed when no brackets present", {

  facets <- plot_facets(test_list)
  vars <- strip_facet_brackets(facets, built_base_plot)
  expect_identical(vars$vars, list(panelvar1 = "carb", panelvar2 = "cyl"))
})

test_that("brackets stripped from facet vars when present", {
  new_test_list <- test_list
  new_test_list$mapping$panelvar2 <- "factor(cyl)"

  facets <- plot_facets(new_test_list)
  vars <- strip_facet_brackets(facets, built_base_plot)
  expect_identical(vars$vars, list(panelvar1 = "carb", panelvar2 = "cyl"))

})


test_that("strip_facet_brackets() doesn't modify vars if they can't be matched to data", {
  wrong_test_list <- test_list
  wrong_test_list$mapping$panelvar1 <- "factor(var_not_in_data)"


  facets <- plot_facets(wrong_test_list)
  vars <- strip_facet_brackets(facets, built_base_plot)
  expect_identical(vars$vars, list(panelvar1 = "factor(var_not_in_data)", panelvar2 = "cyl"))
})



