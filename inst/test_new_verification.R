rm(list=ls())

require(tidyverse)
require(predx)
require(testthat)

verify_BinCat <- function(x, expected, return_df = FALSE,
                            print_output = !return_df) {
  all_exp <- dplyr::bind_rows(lapply(expected,
    expand.grid, stringsAsFactors = F))
  to_unnest <- character()
  if ('cat' %in% names(all_exp)) {
    x$cat <- NA
    x$cat[x$predx_class %in% c('BinCat', 'SampleCat')] <-
      get_cats(x$predx[x$predx_class %in% c('BinCat', 'SampleCat')])
    all_exp$cat[!(all_exp$predx_class %in% c('BinCat', 'SampleCat'))] <- NA
    to_unnest <- c(to_unnest, 'cat')
  }
  if ('lwr' %in% names(all_exp)) {
    x$lwr <- NA
    x$lwr[x$predx_class == 'BinLwr'] <-
      lapply(x$predx[x$predx_class == 'BinLwr'],
        function(x) x@predx[ , 'lwr'])
    all_exp$lwr[all_exp$predx_class != 'BinLwr'] <- NA
    to_unnest <- c(to_unnest, 'lwr')
  }

  all_exp <- unnest(all_exp, cols=to_unnest)
  x <- unnest(x, cols=to_unnest)

  missing_predx <- dplyr::setdiff(all_exp, x[, names(all_exp)])
  if (nrow(missing_predx) > 0 & print_output) {
    print("The following predictions are missing:")
    print(dplyr::as_tibble(missing_predx))
  }

  # check additional predictions found
  additional_predx <- dplyr::setdiff(x[, names(all_exp)], all_exp)
  if (nrow(additional_predx) > 0 & print_output) {
    print("The following additional predictions were found:")
    print(dplyr::as_tibble(additional_predx))
  }

  if (nrow(missing_predx) == 0 & print_output) cat("All expected predictions found.\n")
  if (return_df) {
    return(dplyr::bind_rows(
      dplyr::mutate(missing_predx, status = "missing"),
      dplyr::mutate(additional_predx, status = "additional")
    ))
  }
}


test_that('verification identifies cats', {
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
  expected_demo <- list(
    list(
      location = c('Mercury', 'Venus', 'Earth'),
      target = 'habitability',
      predx_class = 'BinCat',
      cat = list(c('a', 'b', 'c', 'd')))
    )

  expect_equal(capture_output(verify_BinCat(predx_demo, expected_demo)),
    'All expected predictions found.')
  expect_equal(sum(verify_BinCat(predx_demo2,
    expected_demo, return_df=T)$status == 'missing'), 0)
  expect_equal(sum(verify_BinCat(predx_demo2,
    expected_demo, return_df=T)$status == 'additional'), 1)
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

  expect_equal(sum(verify_BinCat(predx_demo,
    expected_demo2, return_df=T)$status == 'missing'), 3)
  expect_equal(sum(verify_BinCat(predx_demo,
    expected_demo2, return_df=T)$status == 'additional'), 0)
  expect_equal(sum(verify_BinCat(predx_demo2,
    expected_demo2, return_df=T)$status == 'missing'), 2)
  expect_equal(sum(verify_BinCat(predx_demo2,
    expected_demo2, return_df=T)$status == 'additional'), 0)
  expect_equal(sum(verify_BinCat(predx_demo2,
    expected_demo3, return_df=T)$status == 'missing'), 6)
  expect_equal(sum(verify_BinCat(predx_demo2,
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

  expect_equal(sum(verify_BinCat(predx_demo_mixed,
    expected_demo, return_df=T)$status == 'missing'), 0)
  expect_equal(sum(verify_BinCat(predx_demo_mixed,
    expected_demo, return_df=T)$status == 'additional'), 3)
  expect_equal(sum(verify_BinCat(predx_demo_mixed,
    expected_demo_mixed, return_df=T)$status == 'missing'), 0)
  expect_equal(sum(verify_BinCat(predx_demo_mixed,
    expected_demo_mixed, return_df=T)$status == 'additional'), 1)
})



