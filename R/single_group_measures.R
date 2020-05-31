
#' Isolation Index
#'
#' The summation is over all the component geographic parts of the larger
#' geographic entity for which the isolation index is calculated.
#'
#' This will report the percentage of population white in the geographic
#' unit, e.g., tract, for the typical or average white person.
#' The maximum value of this isolation index is 100.
#' Even if whites make up only 20 percent of a metropolis population,
#' all of them could live in all-white neighborhoods.
#' The minimum value of the isolation index is asymptotically close to 0.
#' That is, if there is only one white person in a metropolis of 100,000,
#' he or she would live in a geographic unit in which the percent white
#' was close to zero.
#'
#' @details The isolation formula is
#' \eqn{\sigma ((group/sum(group))*(group/totalPop))}
#'
#' @inheritParams divergence
#' @inheritParams dissimilarity
#'
#' @param group A numeric vector of subpopulation totals
#' @param totalPop A numeric vector the length of `group` with population totals.
#'
#' @source Wendell Bell, A Probability Model for the Measurement of Ecological Segregation, Social Forces, Volume 32, Issue 4, May 1954, Pages 357–364, https://doi.org/10.2307/2574118
#' @return A scalar value, see note.
#'
#' @note Setting summed == FALSE will return by-observation measures, but this
#' measure is not meant to be decomposed. These results are for verification
#' purposes only.
#' @export
isolation <- function(group, totalPop, summed=TRUE, na.rm=TRUE){
  iso <- ifelse(totalPop == 0, 0,
    (group / sum(group, na.rm=na.rm)) * (group / totalPop)
  )
  if(summed==T) iso <- sum(iso, na.rm=na.rm)
  return(iso)
}
#' Location Quotient
#'
#' A location quotient (LQ) is an analytical statistic that measures a
#' region’s concentration of a particular attribut relative to a larger geographic
#' unit (e.g. census tract to state). In economics, an
#' LQ is computed as an industry’s share of
#' a regional total for some economic statistic (earnings, GDP by
#' metropolitan area, employment, etc.) divided by the industry’s share
#' of the national total for the same statistic. For example, an LQ of
#' 1.0 in mining means that the region and the nation are equally
#' specialized in mining; while an LQ of 1.8 means that the region has a
#' higher concentration in mining than the nation.
#'
#' @details A simple wrapper for group/groupSum, which must both be given as percentages.
#' @inheritParams divergence
#' @inheritParams isolation
#'
#' @param group a numeric vector giving either a group's percentage of the population in
#' the observation.
#'
#' @param groupSum a number or vector the length of group, the percentage of a group in
#' the overall population.
#'
#' @source U.S. Bureau of Economic Analysis: https://www.bea.gov/help/faq/478
#'
#' @examples
#' data("bay_race")
#' # calculate the location quotient for Hispanic people in each tract in the bay area
#' location_quotient(group = bay_race$hispanic/bay_race$total_pop,
#' groupSum = sum(bay_race$hispanic)/sum(bay_race$total_pop))
#'
#' #alternatively, calculate the location quotient for each tract against an
#' # arbitrary theshold
#'
#'
#' @export
location_quotient <- function(group, groupSum){
  lq <- group/groupSum
  return(lq)
}
