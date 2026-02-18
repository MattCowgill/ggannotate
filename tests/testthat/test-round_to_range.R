test_that("round_to_range rounds appropriately for different ranges", {
  expect_equal(round_to_range(3.14159, c(0, 100)), 3.1)
  expect_equal(round_to_range(3.14159, c(0, 10)), 3.14)
  expect_equal(round_to_range(3.14159, c(0, 1)), 3.142)
  expect_equal(round_to_range(0.0314159, c(0, 0.01)), 0.03142)
})

test_that("round_to_range returns non-numeric values unchanged", {
  date_val <- as.Date("2024-01-15")
  expect_equal(round_to_range(date_val, c(0, 100)), date_val)

  char_val <- "hello"
  expect_equal(round_to_range(char_val, c(0, 100)), char_val)
})

test_that("round_to_range handles zero range", {
  expect_equal(round_to_range(5.123456, c(5, 5)), 5.123456)
})
