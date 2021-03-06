test_that('current entropy calculation matches pre-calculated results',{
  load('bay_results.Rdata')
  load('bay_results_sum.Rdata')

  entropy_score <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, weights = bay_race$total_pop)
  information <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, weights = bay_race$total_pop, entropy_type = 'information_theory')
  entropy_sum <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, weights = bay_race$total_pop, summed = T)
  entropy_scale <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, weights = bay_race$total_pop, scale=T)
  entropy_sum_scale <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, weights = bay_race$total_pop, summed = T, scale=T)
  #entropy values
  expect_equal(entropy_score, bay_results$entropy)
  expect_equal(information, bay_results$entropy_index)
  expect_equal(entropy_scale, bay_results$entropy_scale)
  expect_equal(entropy_sum, bay_results_sum$entropy)
  expect_equal(entropy_sum_scale, bay_results_sum$entropy_scale)

  # formatting
  proper_length <- nrow(bay_race)
  expect_equal(proper_length, length(entropy_score))
  expect_equal(proper_length, length(information))

})
