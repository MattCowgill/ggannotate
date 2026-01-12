test_that("remove_default_params() removes params that match geom defaults", {
  # Use geom_text which has more stable defaults across ggplot2 versions

  params_list <- list(
    colour = "black",
    angle = 0,
    hjust = 0.5,
    vjust = 0.5,
    family = "",
    fontface = 1,
    lineheight = 1.2,
    custom_param = "keep_me"
  )

 params_list_nodefaults <- remove_default_params("geom_text", params_list)

  # custom_param should be kept (not a default)
  expect_true("custom_param" %in% names(params_list_nodefaults))
  expect_equal(params_list_nodefaults$custom_param, "keep_me")

  # Default params should be removed
  expect_false("colour" %in% names(params_list_nodefaults))
  expect_false("angle" %in% names(params_list_nodefaults))
})

test_that("remove_default_params() leaves params that do not match geom defaults", {
  params_list <- list(
    size = 10,
    angle = 45,
    colour = "blue",
    curvature = 0.4,
    arrow = arrow(
      30L,
      unit(
        0.1,
        "inches"
      ),
      "last",
      "closed"
    )
  )

  params_list_nodefaults <- remove_default_params("geom_curve", params_list)

  identical(params_list, params_list_nodefaults)

  expect_identical(params_list_nodefaults, params_list)
})

test_that("remove_default_params() leaves intact params that aren't in defaults", {
  params_list <- list(random_param = "foo")

  expect_identical(
    remove_default_params("geom_text", params_list),
    params_list
  )
})
