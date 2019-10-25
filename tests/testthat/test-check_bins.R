context("Verification of forecast bin specifications")

test_that("check_probs_bins_length_match detects mismatches", {
  expect_equal(check_probs_bins_length_match(5:10, 1:2),
    "the number of bins and the number of probabilities do not match")
  expect_true(check_probs_bins_length_match(1:5, 6:10))
})

test_that("check_bins_ascend detects out of order and reverse order", {
  expect_equal(check_bins_ascend(10:1),
    "the bins are not in ascending order")
  expect_equal(check_bins_ascend(c(5:10, 1:2)),
    "the bins are not in ascending order")
  expect_true(check_bins_ascend(1:10))
})

test_that("check_uniform_bin_size detects varying bin widths", {
  expect_equal(check_uniform_bin_size(c(seq(0, 10, by = 1), 20)),
    "the bin widths are not all equal or are not ordered")
  expect_true(check_uniform_bin_size(seq(0, 20, by = 2)))
})
