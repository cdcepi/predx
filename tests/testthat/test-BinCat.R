context("BinCat")

test_that("Creates BinCat from data frame", {
  test_df <- data.frame(
    cat = c('a', 'b', 'c', 'd'),
    prob = rep(0.25, 4), stringsAsFactors=F)
  expect_is(BinCat(test_df), "BinCat")
})

test_that("BinCat rejects invalid objects", {
  test_df_badsum <- data.frame(
    cat = c('a', 'b', 'c', 'd'),
    prob = rep(0.2, 4), stringsAsFactors=F)
  expect_error(BinCat(test_df_badsum))
  test_df_probgt1 <- data.frame(
    cat = c('a', 'b', 'c', 'd'),
    prob = rep(0.3, 4), stringsAsFactors=F)
  expect_error(BinCat(test_df_probgt1))
  test_df_negprob <- data.frame(
    cat = c('a', 'b', 'c', 'd'),
    prob = c(-0.5, 0.5, 0.5, 0.5), stringsAsFactors=F)
  expect_error(BinCat(test_df_negprob))
  test_df_repbins <- data.frame(
    cat = c('a', 'a', 'c', 'd'),
    prob = rep(0.25, 4), stringsAsFactors=F)
  expect_error(BinCat(test_df_repbins))
})

test_that("BinCat data frame objects convert to predx", {
  test_df <- data.frame(
    cat = c('a', 'b', 'c', 'd'),
    prob = rep(0.25, 4), stringsAsFactors=F)
  expect_silent(to_predx(list(test_df, test_df), rep('BinCat', 2)))
})

test_that("Generics function", {
  this_pred <- BinCat(data.frame(
    cat = c('a', 'b', 'c', 'd'),
    prob = rep(0.25, 4), stringsAsFactors=F))
  expect_equal(names(as.list(this_pred)), c('cat', 'prob'))
  expect_equal(dim(as.data.frame(this_pred)), c(4, 2))
})

