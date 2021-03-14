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
  # maintins compatibility with segregation_measures; segregation report
  divergence_inc_sum <- divergence(bay_race$white,
    bay_race$hispanic,bay_race$asian,bay_race$black, weights = bay_race$total_pop,
    summed = T, rowTotals='weights')
  expect_equal(round(divergence_inc_sum, 4), 0.2096)
})

test_that("divergence works with population percentages",{
  library(dplyr)
  div_percent <- mutate(bay_race, across(c(white, black, hispanic, asian, all_other)),
    list(~(./total_pop))) %>%
    transmute(div_pcts = divergence(white, black, hispanic, asian, all_other, weights = total_pop))

  #scores
  expect_equal(round(div_percent$div_pcts,4), round(bay_results$divergence,4))
})

test_that("A one-row entry returns zero",{
  z <- divergence(bay_race$black[1], bay_race$white[1], population=bay_race$total_pop)
  expect_equal(z, 0)
})
