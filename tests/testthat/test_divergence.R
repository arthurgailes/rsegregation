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
divergence_sum <- divergence(bay_race$white,
  bay_race$hispanic,bay_race$asian,bay_race$black, bay_race$all_other,
  summed = T)

#try incomplete divergence, compatibility with segregation_measures
divergence_inc_sum <- divergence(bay_race$white,
  bay_race$hispanic,bay_race$asian,bay_race$black, weights = bay_race$total_pop,
  summed = T, rowTotals='weights')
expect_equal(round(divergence_inc_sum, 3), 0.219)


# formatting
proper_length <- nrow(bay_race)
expect_equal(proper_length, length(divergence))

#scores
