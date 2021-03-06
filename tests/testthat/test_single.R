
test_that('current results match pre-calculated values',{
  load('bay_results.Rdata')
  load('bay_results_sum.Rdata')
  #test isolation
  iso_white <- isolation(bay_race$white, bay_race$total_pop, summed = T)
  expect_equal(iso_white, bay_results_sum$white_iso)

  #test lq
  white_lq <- location_quotient(bay_race$white, bay_race$total_pop)
  expect_equal(white_lq, bay_results$white_lq)
})

