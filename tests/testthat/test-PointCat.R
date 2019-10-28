context("PointCat")

test_that("Creates PointCat object", {
  expect_is(PointCat('bad season'), "PointCat")
  expect_is(PointCat("1000"), "PointCat")
})

test_that("PointCat rejects invalid point predictions", {
  expect_error(PointCat(9))
  expect_error(PointCat(NA))
  expect_error(PointCat(c('good season','bad season')))
})

test_that("PointCat data frame objects convert to predx", {
  expect_silent(to_predx(list(data.frame(point='testpred'),
    data.frame(point='testpred')), rep('PointCat', 2)))
})

test_that("Generics function", {
  this_pred <- PointCat('testpred')
  expect_equal(predx_to_json(this_pred), list(point = 'testpred'))
  expect_equal(as.data.frame(this_pred), data.frame(point = 'testpred',
    stringsAsFactors = F))
})

test_that("CSV import/export works", {
  fcast <- dplyr::tibble(target = 'x', predx_class = 'PointCat',
    predx = list(PointCat('expected')))
  csv_file <- tempfile()
  export_csv(fcast, csv_file)
  fcast_import <- import_csv(csv_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})

test_that("JSON import/export works", {
  fcast <- dplyr::tibble(target = 'x', predx_class = 'PointCat',
    predx = list(PointCat('expected')))
  json_file <- tempfile()
  export_json(fcast, json_file)
  fcast_import <- import_json(json_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})
