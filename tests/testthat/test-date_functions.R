test_that("check_if_date returns appropriate values", {
  date_plot <- ggplot(ggplot2::economics, aes(x = date, y = unemploy)) +
    geom_line()

  date_plot_built <- ggplot2::ggplot_build(date_plot)

  date_plot_checked <- check_if_date(date_plot_built)

  expect_true(date_plot_checked$x_date)
  expect_false(date_plot_checked$y_date)
  expect_length(date_plot_checked, 2)
  expect_is(date_plot_checked, "list")

  non_date_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  non_date_plot_built <- ggplot2::ggplot_build(non_date_plot)

  non_date_plot_checked <- check_if_date(non_date_plot_built)

  expect_false(non_date_plot_checked$x_date)
  expect_false(non_date_plot_checked$y_date)
  expect_length(date_plot_checked, 2)
  expect_is(date_plot_checked, "list")
})

test_that("num_to_date() returns expected values", {
  expect_identical(num_to_date(12025), as.Date("2002-12-04"))
  expect_error(num_to_date("12025"))
})
