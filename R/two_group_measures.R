#' Dissimilarity Score
#'
#' Stuff
#'
#' @inheritParams divergence
#'
#' @param group1,group2 Numeric vectors representing the popuplation of the
#' groups to be compared.
#'
#' @return A vector the length of group1 & group2
#'
#' @source Duncan, Otis Dudley, and Beverly Duncan. “A Methodological Analysis of Segregation Indexes.” American Sociological Review, vol. 20, no. 2, 1955, pp. 210–217. JSTOR, www.jstor.org/stable/2088328. Accessed 14 Mar. 2020.
#' @export
dissimilarity <- function(group1, group2, .sum=TRUE){
  dissim <- 0.5 * abs(group1/sum(group1, na.rm=T) - group2/sum(group2, na.rm=T))
  (dissim)
}
#' Exposure Index
#'
#' @inheritParams divergence
#' @inheritParams dissimilarity
#'
#' @source Wendell Bell, A Probability Model for the Measurement of Ecological Segregation, Social Forces, Volume 32, Issue 4, May 1954, Pages 357–364, https://doi.org/10.2307/2574118
#'
#' @export
exposure <- function(group1, group2, totalPop){
  expo <- ifelse(totalPop == 0, 0,
    ( (group1/sum(group1, na.rm=T)) * (group2/totalPop) )
  )
  (expo)
}
