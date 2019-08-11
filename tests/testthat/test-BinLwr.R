context("BinLwr")

test_that("Creates BinLwr from matrix or data frame", {
  test_df <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = rep(0.1, 10))
  expect_is(BinLwr(test_df), "BinLwr")
  expect_is(BinLwr(as.matrix(test_df)), "BinLwr")
})

test_that("BinLwr rejects invalid objects", {
  test_df_badsum <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = runif(10))
  expect_error(BinLwr(test_df_badsum))
  test_df_probgt1 <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = c(1.1, 0, 0, 0, 0, 0, 0, 0, 0, 0))
  expect_error(BinLwr(test_df_probgt1))
  test_df_negprob <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = c(-0.5, 0.5, 0.5, 0.5, 0, 0, 0, 0, 0, 0))
  expect_error(BinLwr(test_df_negprob))
  test_df_repbins <- data.frame(
    lwr = c(0.1, seq(0, 0.8, by=0.1)),
    prob = rep(0.1, 10))
  expect_error(BinLwr(test_df_repbins))
  test_df_badbindiff <- data.frame(
    lwr = c(seq(0, 0.7, length.out=5), seq(0.8, 1, length.out=5)),
    prob = rep(0.1, 10))
  expect_error(BinLwr(test_df_badbindiff))
})

test_that("BinLwr data frame objects convert to predx", {
  test_df <- data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = rep(0.1, 10))
  expect_silent(to_predx(list(test_df, test_df), rep('BinLwr', 2)))
})

test_that("Generics function", {
  this_pred <- BinLwr(data.frame(
    lwr = seq(0, 0.9, by=0.1),
    prob = rep(0.1, 10)))
  expect_equal(names(predx_to_json(this_pred)), c('lwr', 'prob'))
  expect_equal(dim(as.data.frame(this_pred)), c(10, 2))
})

test_that("CSV import/export works", {
  fcast <- dplyr::tibble(
    target = c('x', 'y'),
    predx_class = 'BinLwr',
    predx = list(
      BinLwr(data.frame(lwr = c(1, 2), prob = c(0.5, 0.5))),
      BinLwr(data.frame(lwr = c(-5, -4), prob = c(0.3, 0.7)))))
  csv_file <- tempfile()
  export_csv(fcast, csv_file)
  fcast_import <- import_csv(csv_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

test_that("JSON import/export works", {
  fcast <- dplyr::tibble(
    target = c('x', 'y'),
    predx_class = 'BinLwr',
    predx = list(
      BinLwr(data.frame(lwr = c(1, 2), prob = c(0.5, 0.5))),
      BinLwr(data.frame(lwr = c(-5, -4), prob = c(0.3, 0.7)))))
  json_file <- tempfile()
  export_json(fcast, json_file)
  fcast_import <- import_json(json_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

