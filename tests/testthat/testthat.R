library(testthat)
library(rsegregation)
data("alameda_wide")


test_that("Default data works",{
  expect_equal(dim(alameda_wide)[[1]], 361)
})

expect_equal(nrow(alameda_wide), length(divergence(alameda_wide$white,
  alameda_wide$hispanic,alameda_wide$asian,alameda_wide$black)))
