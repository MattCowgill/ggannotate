test_that("check_if_date returns appropriate values", {
  date_plot <- ggplot(ggplot2::economics, aes(x = date, y = unemploy)) +
    geom_line()

  date_plot_built <- ggplot2::ggplot_build(date_plot)

  date_plot_checked <- check_if_date(date_plot_built)

  expect_true(date_plot_checked$x_date)
  expect_false(date_plot_checked$y_date)
  expect_length(date_plot_checked, 2)
  expect_type(date_plot_checked, "list")

  non_date_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  non_date_plot_built <- ggplot2::ggplot_build(non_date_plot)

  non_date_plot_checked <- check_if_date(non_date_plot_built)

  expect_false(non_date_plot_checked$x_date)
  expect_false(non_date_plot_checked$y_date)
  expect_length(date_plot_checked, 2)
  expect_type(date_plot_checked, "list")
})

test_that("num_to_date() returns expected values", {
  expect_identical(num_to_date(12025), as.Date("2002-12-04"))
  expect_null(num_to_date(NULL))
  expect_error(num_to_date("12025"))
})



test_that("correct_scales() corrects scales", {
  p <- ggplot(ggplot2::economics, aes(x = date, y = unemploy)) +
    geom_line() +
    coord_flip()

  built_plot <- ggplot2::ggplot_build(p)

  plot_click <- list(
    x = 9492.364,
    y = 11950.13,
    coords_css = list(
      x = 487,
      y = 108
    )
  )

  corrected_scales <- correct_scales(
    plot_click,
    check_if_date(built_plot),
    get_flipped_coords(built_plot)
  )

  expect_null(corrected_scales$xmin)
  expect_identical(corrected_scales$x, as.Date(plot_click$y, origin = "1970-01-01"))
  expect_identical(corrected_scales$y, plot_click$x)
})
