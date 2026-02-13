test_that("A manual construction of inputs yields an annotated plot", {
  plot <- mtcars %>%
    ggplot(aes(x = wt, y = mpg)) +
    geom_point() +
    facet_wrap(~cyl)

  built_base_plot <- ggplot2::ggplot_build(plot)

  flipped_coords <- get_flipped_coords(built_base_plot)
  axis_classes <- check_if_date(built_base_plot)
  facet_characteristics <- get_facet_characteristics(built_base_plot)

  annot_layer <- 1
  selected_geom <- "text"

  plot_click <- list(
    x = 4,
    y = 23,
    coords_css = list(
      x = 247,
      y = 76
    ),
    coords_img = list(
      x = 424,
      y = 129
    ),
    panelvar1 = 4,
    mapping = list(
      x = "wt",
      y = "mpg",
      panelvar1 = "cyl"
    ),
    domain = list(
      left = 1.32,
      right = 5.62,
      bottom = 9.22,
      top = 35.1
    ),
    range = list(
      left = 316,
      right = 555,
      bottom = 285,
      top = 44.8
    ),
    log = list(
      x = NULL,
      y = NULL
    )
  )

  geom_fn <- switch(
    selected_geom,
    "text" = ggplot2::GeomText,
    "label" = ggplot2::GeomLabel,
    "curve" = ggplot2::GeomCurve,
    "rect" = ggplot2::GeomRect
  )

  known_aes <- geom_fn$aesthetics()

  req_aes <- geom_fn$required_aes

  facets <- get_facets(plot_click)
  facets <- correct_facets(facets, facet_characteristics)

  corrected_scales <- correct_scales(
    plot_click,
    axis_classes,
    flipped_coords
  )

  user_input <- list()

  user_input$facet_vars <- facets$vars
  user_input$facet_levels <- facets$levels
  user_input$x <- corrected_scales$x
  user_input$y <- corrected_scales$y

  params_list <- list()

  annot <- "My annotation"
  annot_no_esc <- gsub("\\n", "\n", annot, fixed = TRUE)

  aes_list <- list(
    x = user_input$x,
    y = user_input$y,
    label = annot_no_esc
  )

  facets_list <- setNames(
    user_input$facet_levels,
    user_input$facet_vars
  )

  this_layer <- list(
    geom = selected_geom,
    aes = aes_list,
    params = params_list,
    facets = facets_list
  ) %>%
    purrr::compact()

  all_layers <- list()

  all_layers[[as.character(annot_layer)]] <- this_layer

  combined_layers <- safely_combine_layers(all_layers)$result

  geom_calls <- purrr::map(
    combined_layers,
    ~ make_layer(
      geom = .x$geom,
      aes = .x$aes,
      facets = .x$facets,
      params = .x$params
    )
  )

  geoms <- purrr::map(geom_calls, eval)

  annot_plot <- plot + geoms

  expect_s3_class(annot_plot, "gg")

  expect_identical(
    annot_plot$layers[[2]]$data,
    data.frame(
      x = 4,
      y = 23,
      label = "My annotation",
      cyl = 4
    )
  )

  vdiffr::expect_doppelganger(
    "faceted mtcars with one text annotation",
    annot_plot
  )
})
