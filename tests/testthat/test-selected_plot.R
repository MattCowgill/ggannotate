library(ggplot2)

test_that("multi-line code string parsed as ggplot2 object", {
  string_1 <- "ggplot(mtcars,
            aes(x = wt, y = mpg)) +
  geom_point()"

  plot_from_string_1 <- text_as_plot(string_1)

  expect_is(plot_from_string_1, "gg")
  expect_equal(selected_plot(string_1), plot_from_string_1)

  vdiffr::expect_doppelganger("plot_from_string_1", plot_from_string_1,
    path = "rstudio-selection"
  )
})


test_that("single line code string parsed as ggplot2 object", {
  string_2 <- "ggplot(mtcars, aes(x = wt, y = mpg)) + geom_point()"

  plot_from_string_2 <- text_as_plot(string_2)

  expect_is(plot_from_string_2, "gg")
  expect_equal(selected_plot(string_2), plot_from_string_2)

  vdiffr::expect_doppelganger("plot_from_string_2", plot_from_string_2,
    path = "rstudio-selection"
  )
})
