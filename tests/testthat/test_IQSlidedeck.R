library(testthat)
library(IQSlides)

test_that("IQSlidedeck", {

  result <- try(source("../testdata/script_IQSlidedeck.R"), silent = TRUE)
  failed <- inherits(result, "try-error")
  expect_false(failed)

})


