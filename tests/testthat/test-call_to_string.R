test_that("call_to_string() returns expected string", {
  mycall <- make_layer("text")

  expect_identical(
    call_to_string(mycall),
    "geom_text(data = data.frame(),\nmapping = aes(),\ninherit.aes = FALSE)"
  )

  myothercall <- make_layer("rect")
  mycalls <- list(mycall, myothercall)

  expect_identical(
    calls_to_string(mycalls),
    "geom_text(data = data.frame(),\nmapping = aes(),\ninherit.aes = FALSE) + \ngeom_rect(data = data.frame(),\nmapping = aes(),\ninherit.aes = FALSE)"
  )
})
