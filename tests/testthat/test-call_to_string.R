test_that("call_to_string() returns short calls on one line", {
  mycall <- make_layer("text")
  expect_identical(
    call_to_string(mycall),
    "geom_text(data = data.frame(), mapping = aes(), inherit.aes = FALSE)"
  )
})

test_that("calls_to_string() joins with +", {
  mycall <- make_layer("text")
  myothercall <- make_layer("rect")
  mycalls <- list(mycall, myothercall)
  expect_identical(
    calls_to_string(mycalls),
    "geom_text(data = data.frame(), mapping = aes(), inherit.aes = FALSE) +\ngeom_rect(data = data.frame(), mapping = aes(), inherit.aes = FALSE)"
  )
})

test_that("call_to_string() formats long calls with 2-space indent", {
  mycall <- make_layer(
    "text",
    aes = list(x = 3.5, y = 28.3, label = "Important point"),
    params = list(colour = "red", hjust = 0, fontface = "bold"),
    facets = list(cyl = 6)
  )
  result <- call_to_string(mycall)
  lines <- strsplit(result, "\n")[[1]]
  expect_gt(length(lines), 1)
  expect_match(lines[1], "^geom_text\\($")
  expect_true(all(grepl("^  ", lines[-c(1, length(lines))])))
  expect_match(lines[length(lines)], "^\\)$")
})

test_that("call_to_string() breaks nested data.frame for combined layers", {
  mycall <- make_layer(
    "text",
    aes = list(
      x = c(2.5, 3.5, 4.5),
      y = c(29, 20, 23),
      label = c("Label A", "Label B", "Label C")
    ),
    params = list(colour = "black")
  )
  result <- call_to_string(mycall)
  expect_match(result, "data\\.frame\\(\n")
  expect_match(result, "  \\)")
})
