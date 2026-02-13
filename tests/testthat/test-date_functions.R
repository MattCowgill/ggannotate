test_that("check_if_date returns appropriate values for Date", {
  date_plot <- ggplot(ggplot2::economics, aes(x = date, y = unemploy)) +
    geom_line()

  date_plot_built <- ggplot2::ggplot_build(date_plot)

  date_plot_checked <- check_if_date(date_plot_built)

  expect_true(date_plot_checked$x_date)
  expect_false(date_plot_checked$y_date)
  expect_false(date_plot_checked$x_datetime)
  expect_false(date_plot_checked$y_datetime)
  expect_length(date_plot_checked, 4)
  expect_type(date_plot_checked, "list")

  non_date_plot <- ggplot(mtcars, aes(x = wt, y = mpg)) +
    geom_point()
  non_date_plot_built <- ggplot2::ggplot_build(non_date_plot)

  non_date_plot_checked <- check_if_date(non_date_plot_built)

  expect_false(non_date_plot_checked$x_date)
  expect_false(non_date_plot_checked$y_date)
  expect_false(non_date_plot_checked$x_datetime)
  expect_false(non_date_plot_checked$y_datetime)
  expect_length(non_date_plot_checked, 4)
  expect_type(non_date_plot_checked, "list")
})

test_that("check_if_date returns appropriate values for datetime (POSIXct)", {
  datetime_df <- data.frame(
    time = as.POSIXct(
      c("2023-01-01 10:00:00", "2023-01-01 12:00:00"),
      tz = "UTC"
    ),
    y = c(1, 2)
  )
  datetime_plot <- ggplot(datetime_df, aes(x = time, y = y)) +
    geom_point()

  datetime_plot_built <- ggplot2::ggplot_build(datetime_plot)
  datetime_plot_checked <- check_if_date(datetime_plot_built)

  expect_false(datetime_plot_checked$x_date)
  expect_false(datetime_plot_checked$y_date)
  expect_true(datetime_plot_checked$x_datetime)
  expect_false(datetime_plot_checked$y_datetime)
})

test_that("num_to_date() returns expected values", {
  expect_identical(num_to_date(12025), as.Date("2002-12-04"))
  expect_null(num_to_date(NULL))
  expect_error(num_to_date("12025"))
})

test_that("num_to_datetime() returns expected values", {
  # 1672531200 is 2023-01-01 00:00:00 UTC
  result <- num_to_datetime(1672531200)
  expect_s3_class(result, "POSIXct")
  expect_equal(
    format(result, "%Y-%m-%d %H:%M:%S", tz = "UTC"),
    "2023-01-01 00:00:00"
  )
  expect_null(num_to_datetime(NULL))
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
  expect_identical(
    corrected_scales$x,
    as.Date(plot_click$y, origin = "1970-01-01")
  )
  expect_identical(corrected_scales$y, plot_click$x)
})
