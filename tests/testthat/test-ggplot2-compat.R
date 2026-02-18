# Tests for ggplot2 compatibility helpers
# These tests ensure ggannotate works with both ggplot2 3.x and 4.0.0+

library(ggplot2)

test_that("get_flipped_coords detects coord_flip", {
  p_normal <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  built_normal <- ggplot_build(p_normal)

  p_flipped <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    coord_flip()
  built_flipped <- ggplot_build(p_flipped)

  expect_false(get_flipped_coords(built_normal))
  expect_true(get_flipped_coords(built_flipped))
})

test_that("is_polar_coord detects polar coordinates", {
  p_normal <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  built_normal <- ggplot_build(p_normal)

  p_polar <- ggplot(mtcars, aes(x = factor(cyl))) +
    geom_bar() +
    coord_polar()
  built_polar <- ggplot_build(p_polar)

  expect_false(is_polar_coord(built_normal))
  expect_true(is_polar_coord(built_polar))
})

test_that("get_panel_scale retrieves scales correctly", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  built <- ggplot_build(p)

  x_scale <- get_panel_scale(built, "x")
  y_scale <- get_panel_scale(built, "y")

  expect_false(is.null(x_scale))
  expect_false(is.null(y_scale))
})

test_that("get_panel_scale detects date scales", {
  p_date <- ggplot(economics, aes(x = date, y = unemploy)) +
    geom_line()
  built_date <- ggplot_build(p_date)

  x_scale <- get_panel_scale(built_date, "x")
  y_scale <- get_panel_scale(built_date, "y")

  expect_true(is_date_scale(x_scale))
  expect_false(is_date_scale(y_scale))
})

test_that("get_facet_params retrieves facet parameters", {
  p_faceted <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~cyl)
  built_faceted <- ggplot_build(p_faceted)

  facet_params <- get_facet_params(built_faceted)

  expect_type(facet_params, "list")
  expect_true(length(facet_params) > 0)
})

test_that("get_facet_params returns empty list for non-faceted plots", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  built <- ggplot_build(p)

  facet_params <- get_facet_params(built)

  expect_type(facet_params, "list")
})

test_that("get_layout_coord retrieves coord object", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  built <- ggplot_build(p)

  coord <- get_layout_coord(built)

  expect_false(is.null(coord))
  expect_true(inherits(coord, "Coord"))
})

test_that("get_geom_defaults retrieves geom defaults", {
  text_defaults <- get_geom_defaults("geom_text")
  label_defaults <- get_geom_defaults("geom_label")

  expect_type(text_defaults, "list")
  expect_type(label_defaults, "list")
  expect_true(length(text_defaults) > 0)
  expect_true(length(label_defaults) > 0)
})

test_that("get_required_aes retrieves required aesthetics", {
  text_geom <- geom_text(aes(x = 1, y = 1, label = "test"))
  text_geom_eval <- eval(text_geom)

  req_aes <- get_required_aes(text_geom_eval)

  expect_type(req_aes, "character")
  expect_true("label" %in% req_aes)
})
