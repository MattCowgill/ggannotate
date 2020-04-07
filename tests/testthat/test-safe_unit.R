test_that("safe_unit returns expected output", {
  expect_null(safe_unit(1, NULL))
  expect_null(safe_unit(NULL, "inches"))
  expect_null(safe_unit(NULL, NULL))

  valid_unit <- safe_unit(1, "inches")
  expect_is(valid_unit, "unit")
  expect_identical(
    as.character(valid_unit),
    "1inches"
  )
})

test_that("safe_arrow returns expected output", {
  expect_null(safe_arrow(30, NULL))
  expect_null(safe_arrow(NULL, 0.1))
  expect_null(safe_arrow(NULL, NULL))

  valid_arrow <- safe_arrow(30, 0.1)
  expect_is(valid_arrow, "arrow")
})
