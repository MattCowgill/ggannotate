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
  expect_type(annot, "language")
  expect_s3_class(annot_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_plot", annot_plot)
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
    facets = list(cyl = 4)
  )
annot_facet_1_plot <- facet_plot_1 +
  eval(annot_facet_1)

test_that("make_layer works with a single facet", {
  expect_type(annot_facet_1, "language")
  expect_s3_class(annot_facet_1_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_facet_1_plot", annot_facet_1_plot)
})

# Flipped bar chart -----

mtcars_with_rownames <- mtcars
mtcars_with_rownames$car <- rownames(mtcars)


flipped_bar_base <- mtcars_with_rownames |>
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
  expect_type(annot_flipped_bar, "language")
  expect_s3_class(annot_flipped_bar_plot, "ggplot")
  # vdiffr::expect_doppelganger("annot_flipped_bar_plot", annot_flipped_bar_plot, path = "make-layer")
})

# Date axis -----
date_line_base <- ggplot(ggplot2::economics, aes(x = date, y = unemploy)) +
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
  expect_type(annot_date_line, "language")
  expect_s3_class(annot_date_line_plot, "ggplot")
  vdiffr::expect_doppelganger(
    title = "annot_date_line_plot",
    fig = annot_date_line_plot
  )
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
  expect_type(annot_curve, "language")
  expect_s3_class(annot_curve_plot, "ggplot")
  vdiffr::expect_doppelganger("annot_curve_plot", fig = annot_curve_plot)
})
