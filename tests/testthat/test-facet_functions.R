
# test_list is a full replica of a list that is returned as `input$plot_click`
# by Shiny, on clicking in a plot
library(ggplot2)

test_base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
  geom_point()


test_base_list <- list(
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
  # panelvar1 = 6,
  # panelvar2 = 0,
  mapping = list(
    x = "wt",
    y = "mpg" # ,
    # panelvar1 = "carb",
    # panelvar2 = "cyl"
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

test_that("get_facet_characteristics() returns expected output", {
  p1 <- ggplot(economics, aes(x = date, y = unemploy)) +
    geom_line()

  expect_length(get_facet_characteristics(ggplot_build(p1)), 0)

  p2 <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  expect_length(get_facet_characteristics(ggplot_build(p2)), 0)

  p3 <- p2 + facet_wrap(~cyl)

  p3_char <- get_facet_characteristics(ggplot_build(p3))

  expect_type(p3_char, "list")
  expect_length(p3_char, 1)
  expect_identical(names(p3_char), "cyl")

  p4 <- p2 + facet_grid(rows = vars(cyl), cols = vars(factor(mpg)))

  p4_char <- get_facet_characteristics(ggplot_build(p4))

  expect_type(p4_char, "list")
  expect_length(p4_char, 2)
  expect_identical(names(p4_char), c("cyl", "factor(mpg)"))
  expect_false(p4_char$cyl$facet_factor)
  expect_true(p4_char$`factor(mpg)`$facet_factor)
})



test_that("plot with no facets treated as expected", {
  facets <- get_facets(test_base_list)

  expect_type(facets, "list")

  expect_length(facets$levels, 0)

  facets <- correct_facets(facets, ggplot2::ggplot_build(test_base_plot))

  expect_type(facets, "list")

  expect_length(facets$levels, 0)
})

test_that("plot with regular facet names treated as expected", {
  facet_plot <- test_base_plot + facet_grid(carb ~ cyl)

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "cyl"

  facets <- get_facets(test_list_facets)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")

  facet_characteristics <- get_facet_characteristics(ggplot2::ggplot_build(facet_plot))
  facets <- correct_facets(facets, facet_characteristics)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
})

test_that("plot with 'factor' in name treated as expected", {
  facet_plot <- test_base_plot + facet_grid(carb ~ factor(cyl))

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "factor(cyl)"

  facets <- get_facets(test_list_facets)
  facet_characteristics <- get_facet_characteristics(ggplot2::ggplot_build(facet_plot))

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "factor(cyl)")
  expect_identical(facets$levels$panelvar2, 3)
  expect_type(facets$levels$panelvar2, "double")

  facets <- correct_facets(facets, facet_characteristics)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_type(facets$levels$panelvar2, "language")
  expect_identical(eval(facets$levels$panelvar2), factor(3))
})

test_that("factor transformation in facet call treated as expected", {
  facet_plot <- test_base_plot + facet_grid(carb ~ factor(cyl, levels = c(2, 4, 6, 8)))

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "factor(cyl, levels = c(2, 4, 6, 8))"

  facets <- get_facets(test_list_facets)
  facet_characteristics <- get_facet_characteristics(ggplot2::ggplot_build(facet_plot))

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "factor(cyl, levels = c(2, 4, 6, 8))")
  expect_identical(facets$levels$panelvar2, 3)
  expect_type(facets$levels$panelvar2, "double")

  facets <- correct_facets(facets, facet_characteristics)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_type(facets$levels$panelvar2, "language")
  expect_identical(eval(facets$levels$panelvar2), factor(3))
})

test_that("factor in raw data for faceting recognised", {
  plot_data <- mtcars
  plot_data$cyl <- factor(plot_data$cyl)

  facet_plot <- ggplot(plot_data, aes(x = wt, y = mpg)) +
    facet_grid(carb ~ cyl)

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "cyl"

  facets <- get_facets(test_list_facets)
  facet_characteristics <- get_facet_characteristics(ggplot2::ggplot_build(facet_plot))


  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_identical(facets$levels$panelvar2, 3)
  expect_type(facets$levels$panelvar2, "double")


  facets <- correct_facets(facets, facet_characteristics)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_type(facets$levels$panelvar2, "language")
  expect_identical(eval(facets$levels$panelvar2), factor(3))
})

test_that("factor with multiple matching columns works as expected", {
  plot_data <- mtcars
  plot_data$cyl2 <- plot_data$cyl
  plot_data$cyl <- factor(plot_data$cyl)

  # Case where there are identical columns (cyl & cyl2) but one (cyl) is
  # unambiguously specified in the facet call
  facet_plot <- ggplot(plot_data, aes(x = wt, y = mpg)) +
    facet_grid(carb ~ cyl)

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "cyl"

  facets <- get_facets(test_list_facets)
  facet_characteristics <- get_facet_characteristics(ggplot2::ggplot_build(facet_plot))

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_identical(facets$levels$panelvar2, 3)
  expect_type(facets$levels$panelvar2, "double")

  facets <- correct_facets(facets, facet_characteristics)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_type(facets$levels$panelvar2, "language")
  expect_identical(eval(facets$levels$panelvar2), factor(3))

  # Case where there are identical columns and one is wrapped in 'factor()'
  facet_plot <- ggplot(plot_data, aes(x = wt, y = mpg)) +
    facet_grid(carb ~ factor(cyl))

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "factor(cyl)"

  facets <- get_facets(test_list_facets)
  facet_characteristics <- get_facet_characteristics(ggplot2::ggplot_build(facet_plot))
  facets <- correct_facets(facets, facet_characteristics)

  expect_identical(eval(facets$levels$panelvar2), factor(3))
  expect_identical(facets$vars$panelvar2, "cyl")

  # Case where there are identical columns and one is wrapped in a longer
  # function call
  facet_plot <- ggplot(plot_data, aes(x = wt, y = mpg)) +
    facet_grid(carb ~ factor(cyl2, levels = c("8", "6", "4")))

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- 'factor(cyl2, levels = c("8", "6", "4"))'

  facets <- get_facets(test_list_facets)
  facet_characteristics <- get_facet_characteristics(ggplot2::ggplot_build(facet_plot))
  facets <- correct_facets(facets, facet_characteristics)
  expect_identical(eval(facets$levels$panelvar2), factor(3))
  expect_identical(facets$vars$panelvar2, "cyl2")
})

test_that("character facets + facet_grid() works", {
  facet1 <- "gear"
  facet2 <- "factor(cyl)"

  base_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()

  p <- base_plot +
    facet_wrap(c(facet1, facet2))

  built_plot <- ggplot_build(p)

  plot_click <- list(
    panelvar1 = 3,
    panelvar2 = 6,
    mapping = list(
      panelvar1 = facet1,
      panelvar2 = facet2
    )
  )

  facets <- get_facets(plot_click)
  facet_characteristics <- get_facet_characteristics(built_plot)

  corrected_facets <- correct_facets(facets, facet_characteristics)

  expect_identical(corrected_facets$vars$panelvar2, "cyl")
  expect_identical(
    eval(corrected_facets$levels$panelvar2),
    factor(6)
  )

  p <- base_plot +
    facet_grid(
      rows = vars(gear),
      cols = vars(factor(cyl))
    )

  built_plot <- ggplot_build(p)
  facets <- get_facets(plot_click)
  facet_characteristics <- get_facet_characteristics(built_plot)

  corrected_facets <- correct_facets(facets, facet_characteristics)

  expect_identical(corrected_facets$levels$panelvar1, 3)
  expect_identical(corrected_facets$vars$panelvar2, "cyl")
  expect_identical(
    eval(corrected_facets$levels$panelvar2),
    factor(6)
  )
})
