context('CSV import/export')

test_that("exported csv is identical when imported (within tolerance)", {
  fcast <- import_flusight_csv('../../vignettes/EW42-Hist-Avg-2018-10-29-National.csv')
  csv_file <- tempfile()
  export_csv(fcast, csv_file)
  fcast_import <- import_csv(csv_file)
  expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
})


