
#' Isolation Index
#'
#' A single-group measure of the degree to which a group is isolated in
#' the full dataset
#'
#' @inheritParams divergence
#'
#' @param group A numeric vector of population
#'
#' @export
isolation <- function(group, totalPop){
  iso <- ifelse(totalPop == 0, 0,
    (group / sum(group, na.rm=T)) * (group / totalPop)
  )
  (iso)
}
#' Location Quotient
#'
#' @inheritParams divergence
#' @inheritParams isolation
#'
#' @export
location_quotient <- function(group, totalPop){
  (group / totalPop) / (sum(group, na.rm=T) / sum(totalPop, na.rm=T))
}
