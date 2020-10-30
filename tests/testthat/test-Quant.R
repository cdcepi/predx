context("Quant")

test_that("Creates Quant from matrix or data frame", {
  test_df <- data.frame(
    quant = seq(0, 1, by=0.1),
    value = 1:11)
  expect_is(Quant(test_df), "Quant")
  expect_is(Quant(as.matrix(test_df)), "Quant")
})

test_that("Quant rejects invalid objects", {
  test_df_nonincreasing <- data.frame(
    quant = seq(0, 0.9, by=0.1),
    value = runif(10))
  expect_error(Quant(test_df_nonincreasing))
  test_df_noninc_q <- data.frame(
    quant = c(0.5, seq(0, 1, by=0.1)),
    value = rep(0, 12))
  expect_error(Quant(test_df_noninc_q))
  test_df_neg_q <- data.frame(
    quant = -1:1,
    value = rep(0, 3))
  expect_error(Quant(test_df_neg_q))
  test_df_gt1_q <- data.frame(
    quant = 0:2,
    value = rep(0, 3))
  expect_error(Quant(test_df_gt1_q))
  test_df_rep_q <- data.frame(
    quant = c(0, 0.5, 0.5, 1),
    value = rep(0, 4))
  expect_error(Quant(test_df_rep_q))
})

test_that("Quant data frame objects convert to predx", {
  test_df <- data.frame(
    quant = seq(0, 1, by=0.1),
    value = 1:11)
  expect_silent(to_predx(list(test_df, test_df), rep('Quant', 2)))
})

test_that("Generics function", {
  this_pred <- Quant(data.frame(
    quant = seq(0, 1, by=0.1),
    value = 1:11))
  expect_equal(names(predx_to_json(this_pred)), c('quant', 'value'))
  expect_equal(dim(as.data.frame(this_pred)), c(11, 2))
})

test_that("CSV import/export works", {
  fcast <- dplyr::tibble(
    target = c('x', 'y'),
    predx_class = 'Quant',
    predx = list(
      Quant(data.frame(quant = seq(0, 1, by=0.1), value = 1:11)),
      Quant(data.frame(quant = seq(0, 1, by=0.1), value = rep(1, 11)))))
  csv_file <- tempfile()
  export_csv(fcast, csv_file)
  fcast_import <- import_csv(csv_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

test_that("JSON import/export works", {
  fcast <- dplyr::tibble(
    target = c('x', 'y'),
    predx_class = 'Quant',
    predx = list(
      Quant(data.frame(quant = seq(0, 1, by=0.1), value = 1:11)),
      Quant(data.frame(quant = seq(0, 1, by=0.1), value = rep(1, 11)))))
  json_file <- tempfile()
  export_json(fcast, json_file)
  fcast_import <- import_json(json_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

