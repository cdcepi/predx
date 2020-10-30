context("Verification of forecast bin specifications")

test_that("check_bins_ascend detects out of order and reverse order", {
  expect_equal(check_ascend(10:1, 'bins'),
    "bins are not all in ascending order")
  expect_equal(check_ascend(c(5:10, 1:2), 'bins'),
    "bins are not all in ascending order")
  expect_true(check_bins_ascend(1:10))
})

test_that("check_uniform_bin_size detects varying bin widths", {
  expect_equal(check_uniform_bin_size(c(seq(0, 10, by = 1), 20)),
    "the bin widths are not all equal or are not ordered")
  expect_true(check_uniform_bin_size(seq(0, 20, by = 2)))
})
