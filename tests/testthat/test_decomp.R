library(testthat)
library(devtools)
library(dplyr)
library(here)
setwd(here())
load_all()
data("bay_race")
load('tests/testthat/bay_results.Rdata')
load('tests/testthat/bay_results_sum.Rdata')

decomp_dplyr <- bay_race %>% select(total_pop:all_other, county) %>%
  decompose_divergence('county',weights = 'total_pop')
decomp_dplyr <- weighted.mean(decomp_dplyr$within + decomp_dplyr$between,
  decomp_dplyr$weights)

decomp_base <- decompose_divergence(subset(bay_race,
  select=c(hispanic:all_other)), groupCol = bay_race$county)
decomp_base <- weighted.mean(decomp_base$within + decomp_base$between,
  decomp_base$weights)

expect_equal(round(decomp_base, 5),round(decomp_dplyr, 5))
expect_equal(round(decomp_base, 5),round(bay_results_sum$divergence, 5))
