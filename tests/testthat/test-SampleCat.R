context("SampleCat")

test_that("Creates SampleCat from character vector", {
  set.seed(42)
  test_vec <- sample(letters[1:3], size = 10, replace = TRUE)
  test_cats <- letters[1:3]
  expect_is(SampleCat(list(sample = test_vec, cat = test_cats)), "SampleCat")
  test_cats <- letters[1:4]
  expect_is(SampleCat(list(sample = test_vec, cat = test_cats)), "SampleCat")
})

test_that("SampleCat rejects invalid objects", {
  set.seed(42)
  test_vec <- c(NA, sample(letters[1:3], size = 10, replace = TRUE))
  test_cats <- letters[1:3]
  expect_error(SampleCat(list(sample = test_vec, cat = test_cats)))
  test_vec <- sample(letters[1:3], size = 10, replace = TRUE)
  test_cats <- letters[1:2]
  expect_error(SampleCat(list(sample = test_vec, cat = test_cats)))
})

test_that("SampleCat data frame objects convert to predx", {
  set.seed(42)
  test_df <- data.frame(
    sample = sample(letters[1:3], size = 10, replace = TRUE),
    stringsAsFactors = FALSE
  )
  attr(test_df, "cat") <- letters[1:3]
  expect_silent(to_predx(list(test_df, test_df), rep('SampleCat', 2)))
})

test_that("Generics function", {
  set.seed(42)
  test_vec <- sample(letters[1:3], size = 10, replace = TRUE)
  test_cats <- letters[1:4]
  this_pred <- SampleCat(list(sample = test_vec, cat = test_cats))
  expect_equal(names(predx_to_json(this_pred)), c('cat', 'sample'))
  expect_equal(dim(as.data.frame(this_pred)), c(10, 1))
  expect_equal(as.data.frame(this_pred)[['sample']], test_vec)
  expect_identical(attr(as.data.frame(this_pred), "cat"), test_cats)
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
  expect_identical(
    get_cats(this_pred),
    test_cats
  )
})

# test_that("CSV import/export works", {
#   fcast <- dplyr::tibble(target = 'x', predx_class = 'SampleCat',
#     predx = list(SampleCat(list(sample = "a", cat = c("a", "b")))))
#   csv_file <- tempfile()
#   export_csv(fcast, csv_file)
#   fcast_import <- import_csv(csv_file)
#   expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
# })
#
# test_that("JSON import/export works", {
#   fcast <- dplyr::tibble(target = 'x', predx_class = 'SampleCat',
#     predx = list(SampleCat(list(sample = "a", cat = c("a", "b")))))
#   fcast <- dplyr::tibble(target = 'x', predx_class = 'SampleCat',
#     predx = list(SampleCat(list(sample = c("a"), cat = c("a", "b")))))
#   json_file <- tempfile()
#   export_json(fcast, json_file)
#   fcast_import <- import_json(json_file)
#   expect_equal(as.data.frame(fcast_import), as.data.frame(fcast))
# })
