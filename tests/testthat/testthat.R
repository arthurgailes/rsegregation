library(testthat)
library(rsegregation)
data("bay_race")


test_that("Default data works",{
  expect_equal(dim(bay_race)[[1]], 1588)
})

divergence <- divergence(bay_race$white,
  bay_race$hispanic,bay_race$asian,bay_race$black)
entropy <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
  bay_race$black, weights = bay_race$total_pop)
information <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
  bay_race$black, weights = bay_race$total_pop, entropy_type = 'information_theory')
entropy_sum <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
  bay_race$black, weights = bay_race$total_pop, summed = T)

# formatting
proper_length <- nrow(bay_race)
expect_equal(proper_length, length(divergence))
expect_equal(proper_length, length(entropy))
expect_equal(proper_length, length(information))
#entropy values
expect_equal(round(entropy_sum, 2), 1.23)
