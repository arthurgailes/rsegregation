# Generate bay area divergence results for testing only
# Only do this when you're sure the results are correct.
library(here)
library(dplyr)
library(devtools)
setwd(here())
load_all()
data("bay_race")

race_tots <- c("hispanic","white","black","asian","all_other")
#calculate all segregation scores for bay area
bay_results <- transmute(bay_race, fips,
  divergence = divergence(!!! syms(race_tots)),
  div_weight = divergence(!!! syms(race_tots), summed = 'weighted'),
  entropy = entropy(!!! syms(race_tots)),
  entropy_sum = entropy(sum(hispanic),sum(white),sum(black),sum(asian),sum(all_other)),
  entropy_scale = entropy(!!! syms(race_tots), scale=T),
  entropy_index = 1 - (entropy/entropy_sum),
  entropy_weight = entropy(!!! syms(race_tots), summed = 'weighted', entropy_type = 'information_theory'),
  white_lq = location_quotient(white/total_pop, sum(white)/sum(total_pop))
)

# summed/bay area results
bay_results_sum <- summarize(bay_race,
  divergence = divergence(!!! syms(race_tots), summed = T),
  entropy = entropy(!!! syms(race_tots), summed = T),
  entropy_scale = entropy(!!! syms(race_tots), summed = T, scale=T),
  entropy_index = entropy(!!! syms(race_tots), entropy_type = 'information_theory',
    summed = T),
  bw_dissim = dissimilarity(black, white,summed = T),
  white_iso = isolation(white, total_pop),
  bw_expo = exposure(black, white, total_pop)
  )

#ensure that the sum of weighted values equals sum values
expect_equal(sum(bay_results$div_weight), bay_results_sum$divergence)
expect_equal(sum(bay_results$entropy_weight), bay_results_sum$entropy_index)

save(bay_results, file = 'tests/testthat/bay_results.Rdata')
save(bay_results_sum, file = 'tests/testthat/bay_results_sum.Rdata')
