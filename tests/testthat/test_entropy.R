test_that('current entropy calculation matches pre-calculated results',{
  load('bay_results.Rdata')
  load('bay_results_sum.Rdata')

  entropy_score <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, population = bay_race$total_pop)
  information <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, population = bay_race$total_pop, entropy_type = 'information_theory')
  entropy_sum <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, population = bay_race$total_pop, summed = T)
  entropy_scale <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, population = bay_race$total_pop, scale=T)
  entropy_sum_scale <- entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
    bay_race$black, bay_race$all_other, population = bay_race$total_pop, summed = T, scale=T)
  #entropy values
  expect_equal(entropy_score, bay_results$entropy)
  expect_equal(information, bay_results$entropy_index)
  expect_equal(entropy_scale, bay_results$entropy_scale)
  # expect_equal(entropy_sum, bay_results_sum$entropy) # original results were wrong, not used
  # expect_equal(entropy_sum_scale, bay_results_sum$entropy_scale) # original results were wrong, not used

  # formatting
  proper_length <- nrow(bay_race)
  expect_equal(proper_length, length(entropy_score))
  expect_equal(proper_length, length(information))

})

test_that("Results match Roberto in Detroit",{
  library(dplyr)
  detroit_mod <- detroit_race %>%
    filter(population>0) %>%
    # get black/white specific stats
    mutate(pop_bw=population*(black+white), black_bw=black/(black+white),
      white_bw=white/(black+white))

  city <- dplyr::filter(detroit_mod, grepl('Detroit',place_name))

  # compare city black/white results
  create_comp <- function(df) summarize(df, ent=entropy(black_bw, white_bw,
    population=pop_bw, summed=T, logBase=2),
    inf=entropy(black_bw, white_bw, entropy_type = 'information_theory',
      population=pop_bw, summed=T, logBase=2))
  city_bw <- create_comp(city)
  expect_equivalent(city_bw$ent, 0.29, tolerance=0.01)
  expect_equivalent(city_bw$inf, 0.32, tolerance=0.01)

  # compare metro results
  metro_bw <- create_comp(detroit_mod)
  expect_equal(metro_bw$ent, 0.33, tolerance=0.01)
  expect_equivalent(metro_bw$inf, 0.59, tolerance=0.01)

})

test_that("Overall entropy reports correct value",{
  library(dplyr)

  overall <- summarize(detroit_race,
    ent=entropy(across(black:nhpi), population=population, entropy_type = 'overall_entropy'),
    across(black:nhpi, weighted.mean, population, na.rm=T)
  )
  ent2 <- select(overall, black:nhpi) %>%
    sapply(function(x) ifelse(x <= 0, 0, x * log(1 / x) )) %>%
    sum

  expect_equal(overall$ent, ent2)
})
