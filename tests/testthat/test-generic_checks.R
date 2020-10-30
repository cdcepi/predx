context("Generic verification checks")

test_that("check_gte0 detects negatives", {
  expect_equal(check_gte0(seq(-1, 1, by = 0.5), 'probs'),
    "there are probs less than zero")
  expect_true(check_probs_gt0(seq(0, 1, by = 0.5)))
})

test_that("check_lte1 detects values > 1", {
  expect_equal(check_lte1(seq(-1, 1.5, by = 0.5), 'probs'),
    "there are probs greater than one")
  expect_true(check_probs_gt0(seq(0, 1, by = 0.5)))
})

