#' Alameda county tract demographics
#'
#' Data by race for each tract in Alameda County, Californai.
#'
#' @source U.S. Census Bureau, 2010 tables P005001, P005003:P005006, P005010. Compiled with tidycensus prackage.
#' @format A data frame with columns:
#' \describe{
#'  \item{fips}{U.S. tract FIPS code}
#'  \item{total_pop}{Total tract population in 2010}
#'  \item{info}{County name and year}
#'  \item{asian,black,hispanic,white,all_other}{Columns by race/ethnicity. All races except hispanic exclude hispanics (i.e. white_2010 is the subset of non-hispanic white people.)}
#' }
#' @examples
#' \dontrun{
#'  alameda_wide
#' }
"alameda_wide"
