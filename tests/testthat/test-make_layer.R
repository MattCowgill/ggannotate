library(ggplot2)
library(vdiffr)
library(tibble)

base_plot <- ggplot(mtcars,
                    aes(x = wt, y = mpg)) +
  geom_point()


# Regular scatterplot ------
annot <- make_layer(geom = "text", x = 3, y = 30, label = "My\ntext")
annot_plot <- base_plot +
  eval(annot)

test_that("make_layer works with basic scatterplot", {
  expect_is(annot, "call")
  expect_is(annot_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_plot", annot_plot, path = "make-layer")
  })

# Faceted scatterplot -----
facet_plot_1 <- base_plot +
  facet_wrap(~cyl)
annot_facet_1 <- make_layer(geom = "text", x = 3, y = 30, label = "My\ntext",
                            facet_var1 = "cyl", facet_level1 = 4)
annot_facet_1_plot <- facet_plot_1 +
  eval(annot_facet_1)

test_that("make_layer works with a single facet", {
  expect_is(annot_facet_1, "call")
  expect_is(annot_facet_1_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_facet_1_plot", annot_facet_1_plot, path = "make-layer")
})

# Flipped bar chart -----
flipped_bar_base <- mtcars %>%
  rownames_to_column("car") %>%
  ggplot(aes(x = reorder(car, mpg),
             y = mpg,
             fill = factor(cyl))) +
  geom_col() +
  coord_flip()

annot_flipped_bar <- make_layer(x = 2, y = 30, label = "My\nannotation", geom = "text")

annot_flipped_bar_plot <- flipped_bar_base +
  eval(annot_flipped_bar)

test_that("make_layer works with flipped coords", {
  expect_is(annot_flipped_bar, "call")
  expect_is(annot_flipped_bar_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_flipped_bar_plot", annot_flipped_bar_plot, path = "make-layer")
})

# Date axis -----
date_line_base <- economics %>%
  ggplot(aes(x = date, y = unemploy)) +
  geom_line()

# annot_date_line <- make_layer()
