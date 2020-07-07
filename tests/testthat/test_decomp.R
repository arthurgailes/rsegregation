library(testthat)
library(devtools)
library(dplyr)
library(here)
setwd(here())
load_all()
data("bay_race")
load('tests/testthat/bay_results.Rdata')
load('tests/testthat/bay_results_sum.Rdata')

#test that the vector of scores lines up
decomp_base <- decompose_divergence(subset(bay_race,
  select=c(hispanic:county)), groupCol = 'county') %>%
  dplyr::mutate(sum = within + between)

expect_equal(sum(decomp_base$sum*decomp_base$weightCol),bay_results_sum$divergence)


## output parameter testing
# 'sum'
decomp_summed <- decompose_divergence(subset(bay_race,
  select=c(hispanic:county)), groupCol = 'county', output='sum')
expect_equal(round(sum(decomp_summed[1:2]),5), round(bay_results_sum$divergence, 5))
# percentage
decomp_perc <- decompose_divergence(subset(bay_race,
  select=c(hispanic:county)), groupCol = 'county', output='percentage')
expect_equal(sum(decomp_perc[1:2]), 1)
# weights
decomp_weight <- decompose_divergence(subset(bay_race,
  select=c(hispanic:county)), groupCol = 'county', output='weighted')
expect_equal(sum(decomp_weight[1:2]), bay_results_sum$divergence)

## test that multiple groups work
bay_race2 <- mutate(bay_race, dumb = rep(1:4,1588/4))
decomp_2gr <- decompose_divergence(subset(bay_race2,
  select=c(hispanic:dumb)), groupCol = c('county','dumb')) %>%
  mutate(sum = within + between) %>%
  filter(county == 'Contra Costa County, California, 2010', dumb == 3)
div_2gr <- mutate(bay_race2, div = divergence(hispanic,white,black,asian,all_other)) %>%
  filter(county == 'Contra Costa County, California, 2010', dumb == 3) %>%
  summarize(div = sum(div*total_pop/sum(total_pop)))


expect_equal(decomp_2gr$sum, sum(div_2gr))
