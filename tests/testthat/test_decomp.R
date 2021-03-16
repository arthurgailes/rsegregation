load('bay_results.Rdata')
load('bay_results_sum.Rdata')


test_that("The sum of decomposed divergence equals the sum of total divergence", {

  decomp_base <- decompose_divergence(subset(bay_race,
    select=c(total_pop:county)), groupCol = 'county', output='all', popCol='total_pop')

  expect_equal(weighted.mean(decomp_base$total,decomp_base$popCol),bay_results_sum$divergence)

})

test_that("the sum of divergence percentage equals 1",{
  decomp_perc <- decompose_divergence(subset(bay_race,
    select=c(total_pop:county)), groupCol = 'county', output='percentage', popCol='total_pop')
  expect_equal(sum(decomp_perc[1:2]), 1)
})

test_that("multiple groups work",{
  library(dplyr)
  bay_race2 <- mutate(bay_race, dumb = rep(1:4,1588/4))
  decomp_2gr <- decompose_divergence(subset(bay_race2,
    select=c(total_pop:dumb)), groupCol = c('county','dumb'), popCol = 'total_pop') %>%
    mutate(sum = within + between) %>%
    filter(county == 'Contra Costa County, California, 2010', dumb == 3)
  div_2gr <- mutate(bay_race2, div = divergence(hispanic,white,black,asian,all_other,
    population=total_pop)) %>%
    filter(county == 'Contra Costa County, California, 2010', dumb == 3) %>%
    summarize(div = sum(div*total_pop/sum(total_pop)))


  expect_equal(decomp_2gr$sum, sum(div_2gr))
})

test_that("'all' parameter returns the correct columns",{
  all_decomp <- decomp_perc <- decompose_divergence(subset(bay_race,
    select=c(total_pop:county)), groupCol = 'county', output='all', popCol='total_pop')

  expect_true(all(c('within','between','within_pct','between_pct','popCol')
    %in% names(all_decomp)))
})

test_that("decomposition works with an NA column",{
  decomp_base <- decompose_divergence(subset(bay_race,
    select=c(total_pop:county)), groupCol = 'county', popCol='total_pop')

  bayNA <- dplyr::mutate(bay_race, new=NA)
  decompNA <- decompose_divergence(subset(bayNA,
    select=c(total_pop:new)), groupCol = 'county', popCol='total_pop')
  expect_equal(decomp_base, decompNA)
})

test_that("decompostition matches Roberto results",{
  load(system.file('extdata','detroit_race.rda', package='rsegregation', mustWork = T))
  library(dplyr)
  # collect decomposed results
  dec <- detroit_race %>%
    select(-tract) %>%
    transmute(place_name = ifelse(grepl('Detroit',place_name), place_name, 'Suburbs'),
      population=population*(white+black), white=white/(white+black), black=black/(white+black)) %>%
    decompose_divergence(groupCol='place_name', popCol = 'population', output='percentage',
      logBase=2) %>%
    select(-place_name) %>% as.data.frame()
  rob_result <- data.frame(within=c(0.05,0.32), between=c(0.5,0.14))

  expect_equal(dec, rob_result, tolerance = 0.03, ignore_attr=T)
})

