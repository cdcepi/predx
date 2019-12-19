rm(list=ls())

require(tidyverse)
require(predx)

predx_demo <- as.predx_df(list(
  location = c('Mercury', 'Venus', 'Earth'),
  target = 'habitability',
  predx = list(
    BinCat(data.frame(
      cat = c('a', 'b', 'c', 'd'),
      prob = rep(0.25, 4), stringsAsFactors=F)),
    BinCat(data.frame(
      cat = c('a', 'b', 'c', 'd'),
      prob = rep(0.25, 4), stringsAsFactors=F)),
    BinCat(data.frame(
      cat = c('a', 'b', 'c', 'd'),
      prob = rep(0.25, 4), stringsAsFactors=F))
  )))

expected_demo <- list(
  list(
    location = c('Mercury', 'Venus', 'Earth'),
    target = 'habitability',
    predx_class = 'BinCat',
    cat = list(c('a', 'b', 'c', 'd')))
  )

predx_demo %>%
  mutate(cats = lapply(predx, get_cats))


x = predx_demo
expected_list = expected_demo
#verify_BinCat <- function(predx, expected) {
  all_exp <- dplyr::bind_rows(lapply(expected_list,
    expand.grid, stringsAsFactors = F))
  if ('cat' %in% names(all_exp)) {
    x$cat <- lapply(x$predx, get_cats)
  }
  missing_predx <- dplyr::setdiff(unnest(all_exp, cols=c('cat')),
    unnest(x[, names(all_exp)], cols=c('cat')))
#}



expect_output(verify_expected(predx_demo, expected_demo))
expect_equal(nrow(verify_expected(predx_demo, expected_demo, return_df=T)), 0)
expected_demo2 <- list(
  list(
    location = c('Mercury', 'Mars'),
    target = 'habitability',
    predx_class = 'Binary'))


