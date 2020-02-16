library(ggplot2)
library(vdiffr)

base_plot <- ggplot(mtcars,
                    aes(x = wt, y = mpg)) +
  geom_point()

annot <- make_layer(geom = "text", x = 3, y = 30, label = "My\ntext")

annot_plot <- base_plot +
  eval(annot)

test_that("make_layer works with basic scatterplot", {

  expect_is(annot, "call")

  expect_is(annot_plot, "ggplot")

})


test_that("annotated plot looks correct", {
  vdiffr::expect_doppelganger("annot_plot", annot_plot, path = NULL)
})
