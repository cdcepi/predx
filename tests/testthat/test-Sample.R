context("Sample")

test_that("Creates Sample from numeric vector", {
  test_vec <- seq(0, 0.9, by=0.1)
  expect_is(Sample(test_vec), "Sample")
})

test_that("Sample rejects invalid objects", {
  test_vec_na <- c(NA, seq(0, 0.9, by=0.1))
  expect_error(Sample(test_vec_na))
  test_vec_char <- letters[1:5]
  expect_error(Sample(test_vec_char))
})

test_that("Sample data frame objects convert to predx", {
  test_df <- data.frame(
    sample = seq(0, 0.9, by=0.1))
  expect_silent(to_predx(list(test_df, test_df), rep('Sample', 2)))
})

test_that("Generics function", {
  this_pred <- Sample(seq(0, 0.9, by=0.1))
  expect_equal(names(predx_to_json(this_pred)), c('sample'))
  expect_equal(dim(as.data.frame(this_pred)), c(10, 1))
  expect_equal(as.data.frame(this_pred)[['sample']], seq(0, 0.9, by=0.1))
  expect_equal(
    quantile(this_pred, probs = c(0.1, 0.4)),
    quantile(seq(0, 0.9, by=0.1), probs = c(0.1, 0.4))
  )
  expect_equal(
    median(this_pred, probs = c(0.1, 0.4)),
    median(seq(0, 0.9, by=0.1), probs = c(0.1, 0.4))
  )
})

test_that("CSV import/export works", {
  fcast <- dplyr::tibble(
    target = c('x', 'y'),
    predx_class = 'Sample',
    predx = list(Sample(c(1, 10)), Sample(c(5, 4))))
  csv_file <- tempfile()
  export_csv(fcast, csv_file)
  fcast_import <- import_csv(csv_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

test_that("JSON import/export works", {
  fcast <- dplyr::tibble(
    target = c('x', 'y'),
    predx_class = 'Sample',
    predx = list(Sample(c(1, 10)), Sample(c(5, 4))))
  json_file <- tempfile()
  export_json(fcast, json_file)
  fcast_import <- import_json(json_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})
