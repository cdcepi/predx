context("Binary")

test_that("Binary accepts probabilities", {
  expect_is(Binary(0.9), "Binary")
  expect_is(Binary(0), "Binary")
  expect_is(Binary(1), "Binary")
})

test_that("Binary rejects non probabilities", {
  expect_silent(Binary(0.9))
  expect_error(Binary("0.9"))
  expect_error(Binary(NA))
  expect_error(Binary(-0.5))
  expect_error(Binary(1.1))
  expect_error(Binary(c(0.3, 0.7)))
})

test_that("Binary data frame objects convert to predx", {
  expect_silent(to_predx(list(data.frame(prob=0.1),
    data.frame(prob=0.5)), rep('Binary', 2)))
})

test_that("Generics function", {
  this_binary <- Binary(0.5)
  expect_equal(predx_to_json(this_binary), list(prob = 0.5))
  expect_equal(as.data.frame(this_binary), data.frame(prob = 0.5))
})

test_that("CSV import/export works", {
  fcast <- dplyr::tibble(target = 'x', predx_class = 'Binary',
    predx = list(Binary(0.5)))
  csv_file <- tempfile()
  export_csv(fcast, csv_file)
  fcast_import <- import_csv(csv_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

test_that("JSON import/export works", {
  fcast <- dplyr::tibble(target = 'x', predx_class = 'Binary',
    predx = list(Binary(0.5)))
  json_file <- tempfile()
  export_json(fcast, json_file)
  fcast_import <- import_json(json_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

