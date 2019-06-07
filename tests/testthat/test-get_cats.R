context("Get categories")

test_that("get_cats works for BinCat and SampleCat", {
  set.seed(42)
  test_vec <- sample(letters[1:3], size = 10, replace = TRUE)
  test_cats <- letters[1:3]
  test_samplecat <- SampleCat(list(sample = test_vec, cat = test_cats))
  expect_identical(get_cats(test_samplecat), test_cats)
  
  test_df <- data.frame(
    cat = c('a', 'b', 'c', 'd'),
    prob = rep(0.25, 4),
    stringsAsFactors = FALSE)
  test_bincat <- BinCat(test_df)
  expect_identical(get_cats(test_bincat), test_df$cat)
})
