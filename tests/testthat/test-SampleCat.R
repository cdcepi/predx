context("SampleCat")

test_that("Creates SampleCat from character vector", {
  set.seed(42)
  test_vec <- sample(letters[1:3], size = 10, replace = TRUE)
  expect_is(SampleCat(test_vec), "SampleCat")
})

test_that("SampleCat rejects invalid objects", {
  set.seed(42)
  test_vec <- c(NA, sample(letters[1:3], size = 10, replace = TRUE))
  expect_error(SampleCat(test_vec))
  test_vec <- sample(1:5, size = 10, replace = TRUE)
  expect_error(SampleCat(test_vec))
})

test_that("Dataframe with SampleCat list converts to predx", {
  set.seed(42)
  test_df = data.frame(sample = sample(letters[1:3], size = 10,
    replace = TRUE), stringsAsFactors = F)
  expect_silent(to_predx(list(test_df, test_df), rep('SampleCat', 2)))
})

test_that("get_predx_colnames extracts all cols", {
  expect_equal(get_predx_colnames('SampleCat'), 'sample')
})

test_that("Generics function", {
  set.seed(42)
  test_vec <- sample(letters[1:3], size = 10, replace = TRUE)
  this_pred <- SampleCat(test_vec)
  expect_equal(names(predx_to_json(this_pred)), 'sample')
  expect_equal(dim(as.data.frame(this_pred)), c(10, 1))
  expect_equal(as.data.frame(this_pred)[['sample']], test_vec)
  expect_equal(
    quantile(this_pred, probs = c(0.1, 0.4)),
    NA
  )
  expect_equal(
    median(this_pred),
    NA
  )
  expect_equal(
    mean(this_pred),
    NA
  )
})

test_that("CSV import/export works", {
  set.seed(42)
  test_vec <- sample(letters[1:3], size = 10, replace = TRUE)
  fcast <- dplyr::tibble(target = 'x', predx_class = 'SampleCat',
    predx = list(SampleCat(test_vec)))
  csv_file <- tempfile()
  export_csv(fcast, csv_file)
  fcast_import <- import_csv(csv_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

test_that("JSON import/export works", {
  set.seed(42)
  test_vec <- sample(letters[1:3], size = 10, replace = TRUE)
  fcast <- dplyr::tibble(target = 'x', predx_class = 'SampleCat',
    predx = list(SampleCat(test_vec)))
  json_file <- tempfile()
  export_json(fcast, json_file)
  fcast_import <- import_json(json_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

# test for original version of SampleCat
test_that("JSON with cats converts to predx with warning", {
  test_json <- '[{
      "target":"x",
      "predx_class":"SampleCat",
      "predx":{
          "sample":["a","a","a","a","b","b","b","a","c","c"],
          "cat":["a","b","c"]}
      }]'
  expect_warning(import_json(test_json))
})
