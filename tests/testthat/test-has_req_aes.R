test_that("has_req_aes() detects when a geom has required aesthetics", {
  expect_false(has_req_aes(geom_text()))
  expect_false(has_req_aes(geom_text(aes(x = 1, y = 2))))

  expect_true(
    has_req_aes(
      geom_text(aes(x = 1, y = 2, label = "something"))
    )
  )
})

test_that("has_req_aes() handles OR syntax in required_aes (e.g., xend|yend)", {
  # GeomCurve requires x, y, and xend|yend (meaning xend OR yend)
  # This was a bug: the function looked for literal "xend|yend" string

  # Curve with both xend and yend should pass
  expect_true(
    has_req_aes(
      geom_curve(aes(x = 1, y = 2, xend = 3, yend = 4))
    )
  )

  # Curve with only xend should also pass (xend OR yend)
  expect_true(
    has_req_aes(
      geom_curve(aes(x = 1, y = 2, xend = 3))
    )
  )

  # Curve missing both xend and yend should fail
  expect_false(
    has_req_aes(
      geom_curve(aes(x = 1, y = 2))
    )
  )
})
