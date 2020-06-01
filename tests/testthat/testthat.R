library(testthat)
library(rsegregation)
data("bay_race")


test_that("Default data works",{
  expect_equal(dim(bay_race)[[1]], 1588)
})

expect_equal(nrow(bay_race), length(divergence(bay_race$white,
  bay_race$hispanic,bay_race$asian,bay_race$black)))
#entropy
t <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
  bay_race$black, weights = bay_race$total_pop, summed = T)
  expect_equal(round(t, 2), 1.23)
