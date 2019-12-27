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

test_that('verification identifies missing cats', {
  predx_demo <- as.predx_df(list(
    location = c('Mercury', 'Venus', 'Earth'),
    target = 'habitability',
    predx = list(
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd'),
        prob = rep(0.25, 4), stringsAsFactors=F)),
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd'),
        prob = rep(0.25, 4), stringsAsFactors=F)),
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd'),
        prob = rep(0.25, 4), stringsAsFactors=F))
    )))
  predx_demo2 <- as.predx_df(list(
    location = c('Mercury', 'Venus', 'Earth'),
    target = 'habitability',
    predx = list(
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd', 'e'),
        prob = rep(0.2, 5), stringsAsFactors=F)),
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd'),
        prob = rep(0.25, 4), stringsAsFactors=F)),
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd'),
        prob = rep(0.25, 4), stringsAsFactors=F))
    )))
  expected_demo2 <- list(
    list(
      location = c('Mercury', 'Venus', 'Earth'),
      target = 'habitability',
      predx_class = 'BinCat',
      cat = list(c('a', 'b', 'c', 'd', 'e')))
    )
  expected_demo3 <- list(
    list(
      location = c('Mercury', 'Venus', 'Earth'),
      target = 'habitability',
      predx_class = 'BinCat',
      cat = list(c('x', 'y')))
    )

  expect_equal(sum(verify_expected(predx_demo,
    expected_demo2, return_df=T)$status == 'missing'), 3)
  expect_equal(sum(verify_expected(predx_demo,
    expected_demo2, return_df=T)$status == 'additional'), 0)
  expect_equal(sum(verify_expected(predx_demo2,
    expected_demo2, return_df=T)$status == 'missing'), 2)
  expect_equal(sum(verify_expected(predx_demo2,
    expected_demo2, return_df=T)$status == 'additional'), 0)
  expect_equal(sum(verify_expected(predx_demo2,
    expected_demo3, return_df=T)$status == 'missing'), 6)
  expect_equal(sum(verify_expected(predx_demo2,
    expected_demo3, return_df=T)$status == 'additional'), 13)
})

test_that('mixed predx objects with cats are verified', {
  predx_demo_mixed <- list(
    location = c('Mercury', 'Venus', 'Earth', 'Earth', 'Venus'),
    target = 'habitability',
    predx = list(
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd', 'e'),
        prob = rep(0.2, 5), stringsAsFactors=F)),
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd'),
        prob = rep(0.25, 4), stringsAsFactors=F)),
      BinCat(data.frame(
        cat = c('a', 'b', 'c', 'd'),
        prob = rep(0.25, 4), stringsAsFactors=F)),
      Point(1),
      BinLwr(data.frame(lwr=c(0, 0.5), prob=c(0.99, 0.01)))
    )) %>%
      as.predx_df()
  expected_demo <- list(
    list(
      location = c('Mercury', 'Venus', 'Earth'),
      target = 'habitability',
      predx_class = 'BinCat',
      cat = list(c('a', 'b', 'c', 'd')))
    )
  expected_demo_mixed <- list(
    list(
      location = c('Mercury', 'Venus', 'Earth'),
      target = 'habitability',
      predx_class = 'BinCat',
      cat = list(c('a', 'b', 'c', 'd'))),
    list(
      location = c('Earth'),
      target = 'habitability',
      predx_class = 'Point'),
    list(
      location = c('Venus'),
      target = 'habitability',
      predx_class = 'BinLwr',
      lwr = c(0, 0.5))
    )

  expect_equal(sum(verify_expected(predx_demo_mixed,
    expected_demo, return_df=T)$status == 'missing'), 0)
  expect_equal(sum(verify_expected(predx_demo_mixed,
    expected_demo, return_df=T)$status == 'additional'), 3)
  expect_equal(sum(verify_expected(predx_demo_mixed,
    expected_demo_mixed, return_df=T)$status == 'missing'), 0)
  expect_equal(sum(verify_expected(predx_demo_mixed,
    expected_demo_mixed, return_df=T)$status == 'additional'), 1)
})


