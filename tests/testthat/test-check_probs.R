context("Verification of probability specifications")

test_that("check_probs_sum_to_one detects > 1", {
  expect_equal(check_probs_sum_to_one(seq(0, 1, by = 0.5)),
    "the probabilities do not sum to 1.0")
  expect_true(check_probs_sum_to_one(c(0, rep(0.1, 8), 0.2)))
})

test_that("check_probs_sum_to_one tolerance can be specified", {
  expect_true(check_probs_sum_to_one(c(0, rep(0.1, 8), 0.22), tolerance = 0.03))
})
