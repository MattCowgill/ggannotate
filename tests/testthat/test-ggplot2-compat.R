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

test_that("strip_panel_size_theme removes panel size overrides", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme(
      panel.heights = grid::unit(5, "cm"),
      panel.widths = grid::unit(5, "cm")
    )
  stripped <- strip_panel_size_theme(p)
  built <- ggplot_build(stripped)
  expect_no_error(ggplot_gtable(built))
})

test_that("strip_panel_size_theme is a no-op for plots without panel size overrides", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  expect_no_error(strip_panel_size_theme(p))
})

test_that("get_fixed_panel_dims returns NULL for plots without fixed panel sizes", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  expect_null(get_fixed_panel_dims(p))
})

test_that("get_fixed_panel_dims detects fixed panel sizes", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point() +
    theme(
      panel.heights = grid::unit(5, "cm"),
      panel.widths = grid::unit(8, "cm")
    )
  dims <- get_fixed_panel_dims(p)
  expect_false(is.null(dims))
  expect_equal(dims$height_cm, 5)
  expect_equal(dims$width_cm, 8)
})

test_that("ggplot2_version returns a package_version", {
  expect_s3_class(ggplot2_version(), "package_version")
})

test_that("is_ggplot2_v4 returns a logical", {
  expect_type(is_ggplot2_v4(), "logical")
})

test_that("is_datetime_scale detects POSIXct scales", {
  df <- data.frame(x = as.POSIXct("2023-01-01") + 0:4 * 3600, y = 1:5)
  built <- ggplot_build(ggplot(df, aes(x = x, y = y)) + geom_line())
  expect_true(is_datetime_scale(get_panel_scale(built, "x")))
  expect_false(is_datetime_scale(NULL))
})

test_that("get_panel_params retrieves panel params with data ranges", {
  p <- ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()
  built <- ggplot_build(p)
  params <- get_panel_params(built)
  expect_false(is.null(params))
  expect_false(is.null(params$x.range))
  expect_false(is.null(params$y.range))
})

test_that("coords_are_normalized returns FALSE for NULL inputs", {
  pp <- list(x.range = c(1, 10), y.range = c(1, 10))
  expect_false(coords_are_normalized(list(x = NULL, y = 0.5), pp))
  expect_false(coords_are_normalized(list(x = 0.5, y = NULL), pp))
  expect_false(coords_are_normalized(list(x = 0.5, y = 0.5), NULL))
  expect_false(coords_are_normalized(
    list(x = 0.5, y = 0.5),
    list(x.range = NULL, y.range = c(1, 10))
  ))
})

test_that("coords_are_normalized detects normalized coordinates", {
  pp <- list(x.range = c(1, 5), y.range = c(10, 30))
  expect_true(coords_are_normalized(list(x = 0.5, y = 0.5), pp))
  expect_false(coords_are_normalized(list(x = 5.0, y = 0.5), pp))
  expect_false(coords_are_normalized(
    list(x = 0.5, y = 0.5),
    list(x.range = c(0, 1), y.range = c(0, 1))
  ))
})

test_that("normalize_to_data_coords returns unchanged for NULL panel_params", {
  click <- list(x = 0.5, y = 0.5)
  expect_equal(normalize_to_data_coords(click, NULL), click)
  expect_equal(
    normalize_to_data_coords(click, list(x.range = NULL, y.range = NULL)),
    click
  )
})

test_that("normalize_to_data_coords converts click coordinates", {
  pp <- list(x.range = c(0, 10), y.range = c(0, 20))
  result <- normalize_to_data_coords(list(x = 0.5, y = 0.25), pp)
  expect_equal(result$x, 5)
  expect_equal(result$y, 5)
})

test_that("normalize_to_data_coords converts brush coordinates", {
  pp <- list(x.range = c(0, 10), y.range = c(0, 20))
  brush <- list(
    x = 0.5,
    y = 0.5,
    xmin = 0.1,
    xmax = 0.9,
    ymin = 0.0,
    ymax = 1.0
  )
  result <- normalize_to_data_coords(brush, pp)
  expect_equal(result$xmin, 1)
  expect_equal(result$xmax, 9)
  expect_equal(result$ymin, 0)
  expect_equal(result$ymax, 20)
})

test_that("convert_to_panel_coords passes through for single-panel plot", {
  built <- ggplot_build(ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point())
  result <- convert_to_panel_coords(0.3, 0.7, built)
  expect_equal(result$x, 0.3)
  expect_equal(result$y, 0.7)
})

test_that("convert_to_panel_coords maps to within-panel coords for faceted plot", {
  built <- ggplot_build(
    ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~am)
  )
  # 2 panels side by side; click at x=0.1 is 20% into left panel
  left <- convert_to_panel_coords(0.1, 0.5, built)
  expect_equal(left$x, 0.2)
  # click at x=0.9 is 80% into right panel
  right <- convert_to_panel_coords(0.9, 0.5, built)
  expect_equal(right$x, 0.8)
})

test_that("infer_facet_from_normalized_coords returns empty for non-faceted plot", {
  built <- ggplot_build(ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point())
  result <- infer_facet_from_normalized_coords(0.5, 0.5, built)
  expect_equal(length(result$vars), 0)
  expect_equal(length(result$levels), 0)
})

test_that("infer_facet_from_normalized_coords identifies facet panel", {
  built <- ggplot_build(
    ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point() + facet_wrap(~am)
  )
  result <- infer_facet_from_normalized_coords(0.25, 0.5, built)
  expect_equal(result$vars$panelvar1, "am")
  expect_true(!is.null(result$levels$panelvar1))
})
