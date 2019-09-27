context("Get categories")

test_that("get_cats works for BinCat and SampleCat", {
  test_df <- data.frame(
    cat = c('a', 'b', 'c', 'd'),
    prob = rep(0.25, 4),
    stringsAsFactors = FALSE)
  test_bincat <- BinCat(test_df)
  expect_identical(get_cats(test_bincat), test_df$cat)
})
