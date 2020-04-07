test_that("call_to_string() returns expected string", {
  mycall <- call("annotate", x = 3, y = 30, geom = "text", label = "test")

  expect_identical(
    call_to_string(mycall),
    "annotate(x = 3,\ny = 30,\ngeom = \"text\",\nlabel = \"test\")"
  )
})
