context('json import/export')

test_that("exported json is identical when imported (within tolerance)", {
  fcast <- import_flusight_csv('../../vignettes/EW42-Hist-Avg-2018-10-29.csv')
  json_file <- tempfile()
  export_json(fcast, json_file)
  fcast_import <- import_json(json_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})


