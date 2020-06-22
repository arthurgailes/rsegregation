library(testthat)
library(devtools)
library(dplyr)
library(here)
setwd(here())
load_all()
data("bay_race")
load('tests/testthat/bay_results.Rdata')
load('tests/testthat/bay_results_sum.Rdata')

proper_length <- nrow(bay_race)
test_that("Default data works",{
  expect_equal(proper_length, 1588)
})

#create figures from package as it is now
div_score <- divergence(bay_race$white,
  bay_race$hispanic,bay_race$asian,bay_race$black, bay_race$all_other)
divergence_sum <- divergence(bay_race$white,
  bay_race$hispanic,bay_race$asian,bay_race$black, bay_race$all_other,
  summed = T)

#try incomplete divergence, compatibility with segregation_measures
divergence_inc_sum <- divergence(bay_race$white,
  bay_race$hispanic,bay_race$asian,bay_race$black, weights = bay_race$total_pop,
  summed = T, rowTotals='weights')
expect_equal(round(divergence_inc_sum, 4), 0.2096)

# test to make sure percentages are working
div_percent <- mutate_at(bay_race, vars(white, black, hispanic, asian, all_other),
  list(~(./total_pop))) %>%
  transmute(div_pcts = divergence(white, black, hispanic, asian, all_other, weights = total_pop))


# formatting

expect_equal(proper_length, length(div_score))

#scores
expect_equal(round(div_percent$div_pcts,4), round(bay_results$divergence,4))
