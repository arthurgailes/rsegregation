library(testthat)
library(devtools)
library(here)
setwd(here())
load_all()
data("bay_race")
load('tests/testthat/bay_results.Rdata')
load('tests/testthat/bay_results_sum.Rdata')


test_that("Default data works",{
  expect_equal(dim(bay_race)[[1]], 1588)
})

#create figures from package as it is now
divergence <- divergence(bay_race$white,
  bay_race$hispanic,bay_race$asian,bay_race$black, bay_race$all_other)
entropy <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
  bay_race$black, bay_race$all_other, weights = bay_race$total_pop)
information <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
  bay_race$black, bay_race$all_other, weights = bay_race$total_pop, entropy_type = 'information_theory')
entropy_sum <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
  bay_race$black, bay_race$all_other, weights = bay_race$total_pop, summed = T)
entropy_sum_scale <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
  bay_race$black, bay_race$all_other, weights = bay_race$total_pop, summed = T, scale=T)

# formatting
proper_length <- nrow(bay_race)
expect_equal(proper_length, length(divergence))
expect_equal(proper_length, length(entropy))
expect_equal(proper_length, length(information))
#entropy values
expect_equal(entropy_sum, bay_results_sum$entropy)
expect_equal(entropy_sum_scale, bay_results_sum$entropy_scale)
