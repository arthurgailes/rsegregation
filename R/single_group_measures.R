
#' Isolation Index
#'
#' The summation is over all the component geographic parts of the larger
#' geographic entity for which the isolation index is calculated.
#'
#' This will report the percentage of population white in the geographic
#' unit, e.g., tract, for the typical or average white person.
#' The maximum value of this isolation index is 100.
#' Even if whites make up only 20 percent of a metropolis’ population,
#' all of them could live in all-white neighborhoods.
#' The minimum value of the isolation index is asymptotically close to 0.  That is, if there is only one white person in a metropolis of 100,000, he or she would live in a geographic unit in which the percent white was close to zero.
#'
#' @inheritParams divergence
#'
#' @param group A numeric vector of population
#'
#' @source Wendell Bell, A Probability Model for the Measurement of Ecological Segregation, Social Forces, Volume 32, Issue 4, May 1954, Pages 357–364, https://doi.org/10.2307/2574118
#'
#' @export
isolation <- function(group, totalPop, .sum=TRUE, na.rm=TRUE){
  iso <- ifelse(totalPop == 0, 0,
    (group / sum(group, na.rm=na.rm)) * (group / totalPop)
  )
  if(.sum==T) iso <- sum(iso, na.rm=na.rm)
  return(iso)
}
#' Location Quotient
#'
#' @inheritParams divergence
#' @inheritParams isolation
#'
#' @export
location_quotient <- function(group, totalPop){
  lq <- (group / totalPop) / (sum(group, na.rm=na.rm) / sum(totalPop, na.rm=na.rm))
  return(lq)
}
