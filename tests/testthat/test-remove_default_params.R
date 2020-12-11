
test_that("remove_default_params() removes params that match geom defaults", {
  params_list <- list(
    size = 0.5,
    angle = 90L,
    colour = "black",
    curvature = 0.5,
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

  expect_identical(
    params_list_nodefaults,
    list(arrow = arrow(
      30L,
      unit(
        0.1,
        "inches"
      ),
      "last",
      "closed"
    ))
  )
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
