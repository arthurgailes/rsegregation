library(testthat)
library(devtools)
library(dplyr)
library(here)
setwd(here())
load_all()
data("bay_race")
load('tests/testthat/bay_results.Rdata')
load('tests/testthat/bay_results_sum.Rdata')

#test that base and dplyr work equivalently
decomp_dplyr <- bay_race %>% select(total_pop:all_other, county) %>%
  decompose_divergence('county',weights = 'total_pop')
decomp_base <- decompose_divergence(subset(bay_race,
  select=c(hispanic:all_other)), groupCol = bay_race$county)


decomp_dplyr_sum <- weighted.mean(decomp_dplyr$within + decomp_dplyr$between,
  decomp_dplyr$weights)
decomp_base_sum <- weighted.mean(decomp_base$within + decomp_base$between,
  decomp_base$weights)

expect_equal(round(decomp_base_sum, 5),round(decomp_dplyr_sum, 5))
expect_equal(decomp_base_sum,bay_results_sum$divergence)


## output parameter testing
# 'sum'
decomp_summed <- decompose_divergence(subset(bay_race,
  select=c(hispanic:all_other)), groupCol = bay_race$county, output='sum')
expect_equal(round(sum(decomp_summed),5), round(bay_results_sum$divergence, 5))
# percentage
decomp_perc <- decompose_divergence(subset(bay_race,
  select=c(hispanic:all_other)), groupCol = bay_race$county, output='percentage')
expect_equal(sum(decomp_perc), 1)
# weights
decomp_weight <- decompose_divergence(subset(bay_race,
  select=c(hispanic:all_other)), groupCol = bay_race$county, output='weighted')
expect_equal(sum(decomp_weight), bay_results_sum$divergence)
