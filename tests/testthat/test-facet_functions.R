
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
  #panelvar1 = 6,
  #panelvar2 = 0,
  mapping = list(
    x = "wt",
    y = "mpg" #,
    #panelvar1 = "carb",
    #panelvar2 = "cyl"
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




test_that("plot with no facets treated as expected", {

  facets <- get_facets(test_base_list)

  expect_is(facets, "list")

  expect_length(facets$levels, 0)

  facets <- correct_facets(facets, ggplot2::ggplot_build(test_base_plot))

  expect_type(facets, "list")

  expect_length(facets$levels, 0)
})

test_that("plot with regular facet names treated as expected", {

  facet_plot <- test_base_plot + facet_grid(carb~cyl)

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "cyl"

  facets <- get_facets(test_list_facets)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")

  facets <- correct_facets(facets, ggplot2::ggplot_build(facet_plot))

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")

})

test_that("plot with 'factor' in name treated as expected", {

  facet_plot <- test_base_plot + facet_grid(carb~factor(cyl))

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "factor(cyl)"

  facets <- get_facets(test_list_facets)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "factor(cyl)")
  expect_identical(facets$levels$panelvar2, 3)
  expect_type(facets$levels$panelvar2, "double")

  facets <- correct_facets(facets, ggplot2::ggplot_build(facet_plot))

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_type(facets$levels$panelvar2, "language")
  expect_identical(eval(facets$levels$panelvar2), factor(3))

})

test_that("factor transformation in facet call treated as expected", {

  facet_plot <- test_base_plot + facet_grid(carb~factor(cyl, levels = c(2, 4, 6, 8)))

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "factor(cyl, levels = c(2, 4, 6, 8))"

  facets <- get_facets(test_list_facets)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "factor(cyl, levels = c(2, 4, 6, 8))")
  expect_identical(facets$levels$panelvar2, 3)
  expect_type(facets$levels$panelvar2, "double")

  facets <- correct_facets(facets, ggplot2::ggplot_build(facet_plot))

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_type(facets$levels$panelvar2, "language")
  expect_identical(eval(facets$levels$panelvar2), factor(3))

})

test_that("factor in raw data for faceting recognised", {
  plot_data <- mtcars
  plot_data$cyl <- factor(plot_data$cyl)

  facet_plot <- ggplot(plot_data, aes(x = wt, y = mpg)) + facet_grid(carb~cyl)

  test_list_facets <- test_base_list
  test_list_facets$panelvar1 <- 6
  test_list_facets$panelvar2 <- 3
  test_list_facets$mapping$panelvar1 <- "carb"
  test_list_facets$mapping$panelvar2 <- "cyl"

  facets <- get_facets(test_list_facets)

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_identical(facets$levels$panelvar2, 3)
  expect_type(facets$levels$panelvar2, "double")

  facets <- correct_facets(facets, ggplot2::ggplot_build(facet_plot))

  expect_type(facets, "list")
  expect_length(facets, 2)
  expect_identical(facets$vars$panelvar2, "cyl")
  expect_type(facets$levels$panelvar2, "language")
  expect_identical(eval(facets$levels$panelvar2), factor(3))

})

### Test correct_facet_var ---

test_that("correct_facet_var() handles normally-specified factors", {
  regular_facet_plot <- test_base_plot +
    facet_wrap(~cyl)

  expect_identical(correct_facet_var("cyl", ggplot_build(regular_facet_plot)),
                   "cyl")

  facet_grid_plot <- test_base_plot +
    facet_grid(rows = "cyl")

  expect_identical(correct_facet_var("cyl", ggplot_build(facet_grid_plot)),
                   "cyl")

  facet_grid_plot_2 <- test_base_plot +
    facet_grid(cols = vars(cyl))

  expect_identical(correct_facet_var("cyl", ggplot_build(facet_grid_plot_2)),
                   "cyl")

})

test_that("correct_facet_var() deals with other facets", {

  factor_facet <- test_base_plot +
    facet_wrap(~factor(cyl))

  expect_identical(correct_facet_var("factor(cyl)", ggplot_build(factor_facet)),
                   "cyl")

  mtcars_2 <- mtcars
  mtcars_2$cyl2 <- mtcars$cyl

  double_var_plot <- ggplot(mtcars_2, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~cyl)

  expect_identical(correct_facet_var("cyl", ggplot_build(double_var_plot)),
                   "cyl")

  double_var_plot <- ggplot(mtcars_2, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~factor(cyl))

  expect_identical(correct_facet_var("factor(cyl)", ggplot_build(double_var_plot)),
                   "cyl")

  double_var_plot <- ggplot(mtcars_2, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~factor(cyl, levels = c("8", "6", "4")))

  expect_error(correct_facet_var('factor(cyl, levels = c("8", "6", "4")',
                                     ggplot_build(double_var_plot)))

})

test_that("correct_facet_class() returns a call to factor() when var is factor", {
  mtcars2 <- mtcars
  mtcars2$cyl2 <- factor(mtcars$cyl)

  expect_type(correct_facet_class("cyl", 6, mtcars2),
              "double")

  expect_identical(
    correct_facet_class("cyl", 6, mtcars2),
    6
  )

  expect_type(correct_facet_class("cyl2", 6, mtcars2),
              "language")


  expect_identical(correct_facet_class("cyl2", 6, mtcars2),
                   call("factor", 6))


})
