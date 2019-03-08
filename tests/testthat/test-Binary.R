context("Binary")

test_that("Binary accepts probabilities", {
  expect_is(Binary(0.9), "Binary")
  expect_is(Binary(0), "Binary")
  expect_is(Binary(1), "Binary")
})

test_that("Binary rejects non probabilities", {
  expect_error(Binary("0.9"))
  expect_error(Binary(NA))
  expect_error(Binary(-0.5))
  expect_error(Binary(1.1))
  expect_error(Binary(c(0.3, 0.7)))
})
