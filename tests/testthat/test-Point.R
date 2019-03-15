context("Point")

test_that("Creates Point object", {
  expect_is(Point(0.9), "Point")
  expect_is(Point(-1000), "Point")
})

test_that("Point rejects invalid point predictions", {
  expect_error(Point("9"))
  expect_error(Point(NA))
  expect_error(Point(c(15, 25)))
})

test_that("Point data frame objects convert to predx", {
  expect_silent(to_predx(list(data.frame(point=10),
    data.frame(point=15)), rep('Point', 2)))
})

test_that("Generics function", {
  this_pred <- Point(5)
  expect_equal(as.list(this_pred), list(point = 5))
  expect_equal(as.data.frame(this_pred), data.frame(point = 5))
})
