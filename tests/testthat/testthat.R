library(testthat)
library(rsegregation)
data("alameda_wide")


test_that("Default data works",{
  expect_equal(dim(alameda_wide)[[1]], 361)
})
