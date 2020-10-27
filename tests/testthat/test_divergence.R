load('bay_results.Rdata')

test_that("Default data works",{
  proper_length <- nrow(bay_race)
  expect_equal(proper_length, 1588)
})

test_that("Current divergence matches previous divergence",{
  div_score <- divergence(bay_race$white,
    bay_race$hispanic,bay_race$asian,bay_race$black, bay_race$all_other)
  divergence_sum <- divergence(bay_race$white,
    bay_race$hispanic,bay_race$asian,bay_race$black, bay_race$all_other,
    summed = T)
  divergence_weight <- divergence(bay_race$white, bay_race$hispanic,
    bay_race$asian,bay_race$black, bay_race$all_other,summed = 'weighted')
  expect_equal(divergence_weight, bay_results$div_weight)
})

test_that("incomplete divergence works",{
  # maintins compatibility with segregation_measures; segregation report
  divergence_inc_sum <- divergence(bay_race$white,
    bay_race$hispanic,bay_race$asian,bay_race$black, weights = bay_race$total_pop,
    summed = T, rowTotals='weights')
  expect_equal(round(divergence_inc_sum, 4), 0.2096)
})

test_that("divergence works with population percentages",{
  library(dplyr)
  div_percent <- mutate(bay_race, across(c(white, black, hispanic, asian, all_other)),
    list(~(./total_pop))) %>%
    transmute(div_pcts = divergence(white, black, hispanic, asian, all_other, weights = total_pop))

  #scores
  expect_equal(round(div_percent$div_pcts,4), round(bay_results$divergence,4))
})


