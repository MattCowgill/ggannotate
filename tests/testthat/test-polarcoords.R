library(ggplot2)

pie <- ggplot(mtcars, aes(x = factor(1), fill = factor(cyl))) +
  geom_bar(width = 1) +
  coord_polar(theta = "y")

test_that("polar coordinates throws an error", {
  expect_error(ggannotate(pie))
})
