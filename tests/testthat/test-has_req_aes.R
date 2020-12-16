test_that("has_req_aes() detects when a geom has required aesthetics", {
  expect_false(has_req_aes(geom_text()))
  expect_false(has_req_aes(geom_text(aes(x = 1, y = 2))))

  expect_true(
    has_req_aes(
      geom_text(aes(x = 1, y = 2, label = "something"))
    )
  )
})
