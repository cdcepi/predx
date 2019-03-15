context("Verification of expected forecasts")

test_that("Missing and unexpected predictions are found", {
  predx_demo <- as.predx_df(list(
    location = c('Mercury', 'Venus', 'Earth'),
    target = 'habitability',
    predx = list(Binary(1e-4), Binary(1e-4), Binary(1))
    ))
  expected_demo <- list(
    list(
      location = c('Mercury', 'Venus', 'Earth'),
      target = 'habitability',
      predx_class = 'Binary')
    )
  expect_output(verify_expected(predx_demo, expected_demo))
  expect_equal(nrow(verify_expected(predx_demo, expected_demo, return_df=T)), 0)
  expected_demo2 <- list(
    list(
      location = c('Mercury', 'Mars'),
      target = 'habitability',
      predx_class = 'Binary'))
  expect_output(verify_expected(predx_demo, expected_demo2))
  expect_equal(nrow(verify_expected(predx_demo, expected_demo2, return_df=T)), 3)
  expect_equal(sum(verify_expected(predx_demo,
    expected_demo2, return_df=T)$status == 'missing'), 1)
  expect_equal(sum(verify_expected(predx_demo,
    expected_demo2, return_df=T)$status == 'additional'), 2)
})
