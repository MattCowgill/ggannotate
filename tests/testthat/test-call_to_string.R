test_that("call_to_string() returns expected string", {
  mycall <- make_layer("text")

  expect_identical(
    call_to_string(mycall),
    "geom_text(data = data.frame(),\nmapping = aes(),\ninherit.aes = FALSE)"
  )
})
