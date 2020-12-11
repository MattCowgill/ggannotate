library(ggplot2)
library(vdiffr)

base_plot <- ggplot(
  mtcars,
  aes(x = wt, y = mpg)
) +
  geom_point()


# Regular scatterplot ------
annot <-
  make_layer(
    geom = "text",
    aes = list(
      x = 3,
      y = 30,
      label = "My\ntext"
    )
  )
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
annot_facet_1 <-
  make_layer(
    geom = "text",
    aes = list(
      x = 3,
      y = 30,
      label = "My\ntext"
    ),
    facet_vars = list("cyl"),
    facet_levels = list(4)
  )
annot_facet_1_plot <- facet_plot_1 +
  eval(annot_facet_1)

test_that("make_layer works with a single facet", {
  expect_is(annot_facet_1, "call")
  expect_is(annot_facet_1_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_facet_1_plot", annot_facet_1_plot, path = "make-layer")
})

# Flipped bar chart -----

mtcars_with_rownames <- mtcars
mtcars_with_rownames$car <- rownames(mtcars)


flipped_bar_base <- mtcars_with_rownames %>%
  ggplot(aes(
    x = reorder(car, mpg),
    y = mpg,
    fill = factor(cyl)
  )) +
  geom_col() +
  coord_flip()

annot_flipped_bar <-
  make_layer(
    aes = list(
      x = 2,
      y = 30,
      label = "My\nannotation"
    ),
    geom = "text"
  )

annot_flipped_bar_plot <- flipped_bar_base +
  eval(annot_flipped_bar)

test_that("make_layer works with flipped coords", {
  expect_is(annot_flipped_bar, "call")
  expect_is(annot_flipped_bar_plot, "ggplot")
  # vdiffr::expect_doppelganger("annot_flipped_bar_plot", annot_flipped_bar_plot, path = "make-layer")
})

# Date axis -----
date_line_base <- economics %>%
  ggplot(aes(x = date, y = unemploy)) +
  geom_line()

annot_date_line <-
  make_layer(
    geom = "text",
    aes = list(
      x = as.Date("1982-06-08"),
      y = 12781.0431364006,
      label = "My annotation"
    )
  )

annot_date_line_plot <- date_line_base +
  eval(annot_date_line)

test_that("make_layer works with date axes", {
  expect_is(annot_date_line, "call")
  expect_is(annot_date_line_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_date_line_plot", annot_date_line_plot, path = "make-layer")
})

# Curve ----

annot_curve <- make_layer(
  geom = "curve",
  aes = list(
    x = 2.20835965521693,
    y = 31.8618676116827,
    xend = 3.13939564982646,
    yend = 24.375414662927
  )
)

annot_curve_plot <- base_plot +
  eval(annot_curve)

test_that("make_layer works with geom_curve", {
  expect_is(annot_curve, "call")
  expect_is(annot_curve_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_curve_plot", annot_curve_plot, path = "make-layer")
})
