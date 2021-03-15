load('bay_results.Rdata')

test_that("Default data works",{
  proper_length <- nrow(bay_race)
  expect_equal(proper_length, 1588)
})

test_that("Results match Roberto in Detroit",{
  data("detroit_race")
  library(dplyr)
  detroit_mod <- detroit_race %>%
    filter(population>0) %>%
    # get black/white specific stats
    mutate(pop_bw=population*(black+white), black_bw=black/(black+white),
      white_bw=white/(black+white))

    city <- dplyr::filter(detroit_mod, grepl('Detroit',place_name))

    # compare city black/white results
    city_bw <- summarize(city, div=divergence(black_bw, white_bw,
      population=pop_bw, summed=T, logBase=2))
    expect_equivalent(city_bw$div, 0.14, tolerance=0.01)

    # compare metro results
    metro_bw <- summarize(detroit_mod, div=divergence(black_bw, white_bw,
      population=pop_bw, summed=T, logBase=2))
    expect_equal(metro_bw$div, 0.48, tolerance=0.01)

})

test_that("Current divergence matches previous divergence",{
  div_score <- divergence(bay_race$white,
    bay_race$hispanic,bay_race$asian,bay_race$black, bay_race$all_other)
  divergence_sum <- divergence(bay_race$white,
    bay_race$hispanic,bay_race$asian,bay_race$black, bay_race$all_other,
    summed = T)
  divergence_weight <- divergence(bay_race$white, bay_race$hispanic,
    bay_race$asian,bay_race$black, bay_race$all_other,summed = 'weighted')
  expect_equal(divergence_weight, bay_results$div_weight)
})

test_that("incomplete divergence works",{
  # maintains compatibility with segregation_measures; segregation report
  t <- bay_race$white+bay_race$hispanic+bay_race$black+bay_race$asian
  bay_race2 <- mutate(bay_race, across(hispanic:asian, ~(.x/t)), total_pop=total_pop*t)
  divergence_inc_sum <- summarize(bay_race2, div=divergence(across(hispanic:asian), population=total_pop, summed=T),
    across(hispanic:asian, weighted.mean, total_pop, na.rm=T))
  expect_equal(round(divergence_inc_sum, 4), 0.2096)
})


test_that("A one-row entry returns zero",{
  z <- suppressWarnings(divergence(bay_race$black[1], bay_race$white[1], population=bay_race$total_pop[1]))
  expect_equal(z, 0)
})
