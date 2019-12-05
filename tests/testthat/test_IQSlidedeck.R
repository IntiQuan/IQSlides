library(testthat)
library(IQSlides)


mywd <- getwd()
setwd(tempdir())
unlink("../Output", recursive = TRUE)
setwd(mywd)

test_that("IQSlidedeck Base", {

  result <- try(source("../testdata/script_IQSlidedeck.R"), silent = TRUE)
  failed <- inherits(result, "try-error")
  expect_false(failed)

})


test_that("IQSlidedeck IQR Output Objects", {

  result <- try(source("../testdata/script_IQSlidedeck_IQRobjects.R"), silent = TRUE)
  failed <- inherits(result, "try-error")
  expect_false(failed)

})

