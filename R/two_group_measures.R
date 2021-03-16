#' Dissimilarity Score
#'
#' This index measures the evenness with which two mutually exclusive
#' groups are distributed across the geographic units that make up a
#' larger geographic entity; for example, the distribution of blacks and
#' whites across the census tracts that make up a metropolis.
#' Its minimum value is zero and its maximum value is 100.
#'
#'
#' @inheritParams divergence
#'
#' @param group1,group2 Numeric vectors representing the proportions of each group in each observation
#'
#' @param population if `group1` and `group2` are proportions, this parameter gives the population weights.
#'  Set to 1 to avoid a warning measure if using population
#'
#'
#' @return A scalar value, see note.
#'
#' @source Duncan, Otis Dudley, and Beverly Duncan. A Methodological Analysis of Segregation Indexes.” American Sociological Review, vol. 20, no. 2, 1955, pp. 210–217. JSTOR, www.jstor.org/stable/2088328. Accessed 14 Mar. 2020.
#'
#' @note Setting summed == FALSE will return by-observation measures, but this
#' measure is not meant to be decomposed. These results are for verification
#' purposes only.
#'
#' @export
dissimilarity <- function(group1, group2, population = NA, summed=TRUE, na.rm=TRUE){
  population <- multigroup_population(groupMatrix=matrix(group1), population=population, na.rm=na.rm)
  dissim <- 0.5 * abs(group1*population/sum(group1*population, na.rm=na.rm) -
      group2*population/sum(group2*population, na.rm=na.rm))
  if(summed==T) dissim <- sum(dissim, na.rm=na.rm)
  return(dissim)
}
#' Exposure Index
#'
#' The summation is over all the geographic units, e. g. census tracts,
#' comprising the larger geographic entity for which the exposure index
#' is being calculated.
#'
#' The maximum value of the exposure index is the percent in the second
#' group.  That is, if blacks make up 30 percent of the population of a
#' metropolis, the maximum value of the average percent black for white
#' residents of that metropolis will be 30 percent.  This will require
#' that the index of dissimilarity measuring the evenness with which
#' blacks and every other racial group are distributed across the
#' metropolis equals zero. The minimum value of the exposure index is zero.  That is, although blacks might make up 30 percent of a metropolis, whites could live in exclusively white neighborhoods.   If the exposure index equals zero, than the index of dissimilarity comparing those two groups will equal zero. The exposure index involves two mutually exclusive racial groups.  However, the average percent black in the census tract of the typical white in a metropolis is almost always different from the average percent white for the typical black living in the same metropolis.
#'
#' @details The formula for exposure is \eqn{\Sigma (g1/G1)*(g2/t)},
#' where g1=group1, G1=sum(group1),g2=group2, and t=totalPop
#' @inheritParams divergence
#' @inheritParams dissimilarity
#' @inheritParams isolation
#'
#' @source Wendell Bell, A Probability Model for the Measurement of Ecological Segregation, Social Forces, Volume 32, Issue 4, May 1954, Pages 357–364, https://doi.org/10.2307/2574118
#'
#' @return A scalar value, see note.
#'
#' @note Setting summed == FALSE will return by-observation measures, but this
#' measure is not meant to be decomposed. These results are for verification
#' purposes only.
#'
#' @export
exposure <- function(group1, group2, population=NA, summed=TRUE, na.rm=TRUE){
  population <- multigroup_population(groupMatrix=matrix(group1), population=population, na.rm=na.rm)
  expo <- ifelse(population*group1 == 0, 0,
    ( (group1*population/sum(group1*population, na.rm=na.rm)) * group2 )
  )
  if(summed==T) expo <- sum(expo, na.rm=na.rm)
  return(expo)
}
