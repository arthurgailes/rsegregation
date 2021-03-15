test_that('two-race measures match previous bay-area results', {
  load('bay_results.Rdata')
  load('bay_results_sum.Rdata')

  bw_diss <- dissimilarity(bay_race$white, bay_race$black, population=bay_race$total_pop)
  bw_diss2 <- dissimilarity(bay_race$white*bay_race$total_pop, bay_race$black*bay_race$total_pop, pop=1)
  expect_equal(bw_diss, bay_results_sum$bw_dissim)
  expect_equal(bw_diss, bw_diss2)

  bw_expo <- exposure(bay_race$black, bay_race$white, population=bay_race$total_pop)
  bw_expo2 <- exposure(bay_race$black*bay_race$total_pop, bay_race$white, population=1)
  expect_equal(bw_expo, bay_results_sum$bw_expo)
  expect_equal(bw_expo, bw_expo2)
})
