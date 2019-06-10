context("Transform predx class")

test_that("transform_predx works for Sample to BinLwr", {
  lwr <- seq(0, 9.9, by=0.1)
  x <- c(1.1, 1.15, 1.199999, 1.2, 3.3)
  
  test_sample <- Sample(x)
  test_binlwr <- transform_predx(test_sample, class = "BinLwr", lwr = lwr)
  expected <- data.frame(
    lwr = lwr,
    prob = 0
  )
  expected$prob[abs(expected$lwr - 1.1) < 0.001] <- 3/5
  expected$prob[abs(expected$lwr - 1.2) < 0.001] <- 1/5
  expected$prob[abs(expected$lwr - 3.3) < 0.001] <- 1/5
  
  expect_identical(as.data.frame(test_binlwr), expected)
})

test_that("transform_predx fails for Sample to BinLwr with samples outside bins", {
  lwr <- seq(0, 9.9, by=0.1)
  x <- c(1.1, 1.15, 1.199999, 1.2, 3.3, 10.1)
  
  test_sample <- Sample(x)
  expect_error(transform_predx(test_sample, class = "BinLwr", lwr = lwr))
})

test_that("transform_predx fails for Sample to BinLwr with unequal binwidths", {
  lwr <- c(seq(0, 9.9, by=0.1), 10.5)
  x <- c(1.1, 1.15, 1.199999, 1.2, 3.3, 10.1)
  
  test_sample <- Sample(x)
  expect_error(transform_predx(test_sample, class = "BinLwr", lwr = lwr))
})



test_that("transform_predx works for SampleCat to BinCat", {
  test_vec <- c("a", "b", "b", "c", "a", "a")
  test_cats <- letters[1:4]
  test_samplecat <- SampleCat(list(sample = test_vec, cat = test_cats))
  test_bincat <- transform_predx(test_samplecat, class = "BinCat", cat = test_cats)
  
  expected <- data.frame(
    cat = letters[1:4],
    prob = (3:0)/6,
    stringsAsFactors = FALSE
  )
  rownames(expected) <- test_cats
  
  expect_equal(as.data.frame(test_bincat), expected)
})

test_that("transform_predx fails for SampleCat to BinCat if categories don't match", {
  test_vec <- c("a", "b", "b", "c", "a", "a")
  test_cats <- letters[1:4]
  test_samplecat <- SampleCat(list(sample = test_vec, cat = test_cats))
  expect_error(transform_predx(test_samplecat, class = "BinCat", cat = letters[1:3]))
})



test_that("transform_predx works on lists", {
  test_cats <- letters[1:4]
  test_vec <- c("a", "b", "b", "c", "a", "a")
  test_samplecat <- SampleCat(list(sample = test_vec, cat = test_cats))
  
  test_vec <- c("a", "b", "b", "c", "a", "a", "d")
  test_samplecat2 <- SampleCat(list(sample = test_vec, cat = test_cats))
  
  test_bincat <- data.frame(
      cat = letters[1:4],
      prob = (3:0)/6,
      stringsAsFactors = FALSE
    ) %>%
    BinCat()
  
  test_list_in <- list(test_samplecat, test_samplecat2, test_bincat)
  test_list_out <- transform_predx(test_list_in, class = "BinCat", cat = test_cats)
  
  expected_out <- list(
    transform_predx(test_samplecat, class = "BinCat", cat = test_cats),
    transform_predx(test_samplecat2, class = "BinCat", cat = test_cats),
    test_bincat
  )
  
  expect_identical(test_list_out, expected_out)
})
