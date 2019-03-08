context("BinLwr")

test_that("BinLwr accepts a matrix or data frame", {
  test_df <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = rep(0.1, 10))
  expect_is(BinLwr(test_df), "BinLwr")
  expect_is(BinLwr(as.matrix(test_df)), "BinLwr")
})

test_that("BinLwr detects sum != 1", {
  test_df_badsum <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = runif(10))
  expect_error(BinLwr(test_df_badsum))
})

test_that("BinLwr requires 0 <= prob <= 1", {
  test_df_probgt1 <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = c(1.1, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  expect_error(BinLwr(test_df_probgt1))
  test_df_negprob <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = c(-0.5, 0.5, 0.5, 0.5, 0, 0, 0, 0, 0, 0))
  expect_error(BinLwr(test_df_negprob))
})

test_that("BinLwr detects problems with bins", {
  test_df_repbins <- data.frame(
    lwr = c(0.1, seq(0, 0.8, by=0.1)),
    prob = rep(0.1, 10))
  expect_error(BinLwr(test_df_repbins))
  test_df_badbindiff <- data.frame(
    lwr = c(seq(0, 0.7, length.out=5), seq(0.8, 1, length.out=5)),
    prob = rep(0.1, 10))
  expect_error(BinLwr(test_df_badbindiff))
})
