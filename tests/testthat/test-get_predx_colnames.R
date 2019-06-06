context("Work with predx classes")

test_that("get_predx_colnames extracts all cols", {
  expect_equal(get_predx_colnames('Binary'), 'prob')
  expect_equal(get_predx_colnames(c('Binary', 'Point')), c('prob', 'point'))
  expect_true(all(get_predx_colnames(c('BinCat', 'BinLwr', 'Binary')) %in%
    c('prob', 'cat', 'lwr')))
  expect_equal(get_predx_colnames('Sample'), 'sample')
})
