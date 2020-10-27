load('bay_results.Rdata')
load('bay_results_sum.Rdata')


test_that("The sum of decomposed divergence equals the sum of total divergence", {

  decomp_base <- decompose_divergence(subset(bay_race,
    select=c(hispanic:county)), groupCol = 'county', output = 'weighted')

  expect_equal(sum(decomp_base$total*decomp_base$weightCol),bay_results_sum$divergence)

})

test_that("the sum of divergence percentage equals 1",{
  decomp_perc <- decompose_divergence(subset(bay_race,
    select=c(hispanic:county)), groupCol = 'county', output='percentage')
  expect_equal(sum(decomp_perc[1:2]), 1)
})

test_that("multiple groups work",{
  library(dplyr)
  bay_race2 <- mutate(bay_race, dumb = rep(1:4,1588/4))
  decomp_2gr <- decompose_divergence(subset(bay_race2,
    select=c(hispanic:dumb)), groupCol = c('county','dumb')) %>%
    mutate(sum = within + between) %>%
    filter(county == 'Contra Costa County, California, 2010', dumb == 3)
  div_2gr <- mutate(bay_race2, div = divergence(hispanic,white,black,asian,all_other)) %>%
    filter(county == 'Contra Costa County, California, 2010', dumb == 3) %>%
    summarize(div = sum(div*total_pop/sum(total_pop)))


  expect_equal(decomp_2gr$sum, sum(div_2gr))
})

