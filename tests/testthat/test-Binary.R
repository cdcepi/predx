context("Binary")

test_that("Binary accepts probabilities", {
  expect_is(Binary(0.9), "Binary")
  expect_is(Binary(0), "Binary")
  expect_is(Binary(1), "Binary")
})

test_that("Binary rejects non probabilities", {
  expect_silent(Binary(0.9))
  expect_error(Binary("0.9"))
  expect_error(Binary(NA))
  expect_error(Binary(-0.5))
  expect_error(Binary(1.1))
  expect_error(Binary(c(0.3, 0.7)))
})

test_that("Binary data frame objects convert to predx", {
  expect_silent(to_predx(list(data.frame(prob=0.1),
    data.frame(prob=0.5)), rep('Binary', 2)))
})

test_that("Generics function", {
  this_binary <- Binary(0.5)
  expect_equal(as.list(this_binary), list(prob = 0.5))
  expect_equal(as.data.frame(this_binary), data.frame(prob = 0.5))
})
