
#' Isolation Index
#'
#' A single-group measure of the degree to which a group is isolated in
#' the full dataset
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
    (group / sum(group, na.rm=T)) * (group / totalPop)
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
  lq <- (group / totalPop) / (sum(group, na.rm=T) / sum(totalPop, na.rm=T))
  return(lq)
}
