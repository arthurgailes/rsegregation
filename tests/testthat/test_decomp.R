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

test_that("'all' parameter returns the correct columns",{
  all_decomp <- decomp_perc <- decompose_divergence(subset(bay_race,
    select=c(hispanic:county)), groupCol = 'county', output='all')

  expect_true(all(c('within','between','within_pct','between_pct','weightCol')
    %in% names(all_decomp)))
})

test_that("decomposition works with an NA column",{
  decomp_base <- decompose_divergence(subset(bay_race,
    select=c(hispanic:county)), groupCol = 'county')

  bayNA <- dplyr::mutate(bay_race, new=NA)
  decompNA <- decompose_divergence(subset(bayNA,
    select=c(hispanic:new)), groupCol = 'county')
  expect_equal(decomp_base, decompNA)
})

test_that("decompostition matches Roberto results",{
  library(dplyr)
  # collect decomposed results
  dec <- detroit_race %>%
    select(-tract) %>%
    transmute(place_name = ifelse(grepl('Detroit',place_name), place_name, 'Suburbs'),
      population=population*(white+black), white=white/(white+black), black=black/(white+black)) %>%
    decompose_divergence(groupCol='place_name', weightCol = 'population', output='percentage',
      logBase=2) %>%
    select(-place_name) %>% as.data.frame()
  rob_result <- data.frame(within=c(0.05,0.32), between=c(0.5,0.14))

  expect_equal(dec, rob_result, tolerance = 0.03, ignore_attr=T)
})

