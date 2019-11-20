library(testthat)
library(IQSlides)

test_that("splitNumNames", {

  files <- c("1_bla", "2_bla_bli", "3_bla_bli_blub")
  result <- splitNumNames(files)

  expect_equal(result$prefix, c(1, 2, 3))
  expect_equal(as.character(result$name), c("bla", "bla_bli", "bla_bli_blub"))
  expect_true(result$OK_prefix)
  expect_equal(result$original, files)

})


