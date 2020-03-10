#' Divergence Index
#'
#' Calculates the divergence index of segregation
#'
#' @param totalPop The total population of the geography (e.g. Census
#' tract) being analyzed. If not specified, deaults to the sum of the
#' populations provided in `...`
#'
#' @param ... columns to be included in the calculation of the index.
#'
#' @param .sum If TRUE, will return one value. (Or one value per group if specifying
#' `dplyr::group_by`.) If FALSE, will return a vector equaling the length
#' of the input vectors.
#'
#' @examples
#' divergence(alameda_wide$white,alameda_wide$hispanic,alameda_wide$asian,
#' alameda_wide$black, totalPop = alameda_wide$total_pop)
#'
#' \dontrun{
#' # Entering dataframe will cause an error
#' divergence(alameda_wide[c("white","black","asian","hispanic")])
#' }
#'
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @source Created by Elizabeth Roberto: <https://arxiv.org/abs/1508.01167>
#' @export
divergence <- function(..., totalPop = NULL, na.rm=TRUE, .sum=FALSE){

  races <- list(...)

  # convert list of vectors into DF, then convert to percentages
  raceMatrix <- as.data.frame(do.call(cbind, races))
  raceMatrix[is.na(raceMatrix)] <- 0
  #If total popuation is not provided, create from the sum of races
  if(is.null(totalPop)) totalPop <- apply(raceMatrix, 1, sum)
  raceMatrix <- raceMatrix / totalPop

  # Create empty matrix with one column for each race
  dat <- matrix(nrow = nrow(raceMatrix), ncol = length(races))
  #each item in ... should be a matrix of race totals
  i = 0
  for(race in races){
    # create race proportion
    race_bigGeo <- sum(race, na.rm=TRUE) / sum(totalPop, na.rm=TRUE)
    race <- ifelse(totalPop == 0, 0, race / totalPop)
    i = i + 1
    score <- ifelse(race <= 0 | race_bigGeo <= 0, 0,
      race * log(race / race_bigGeo) )
    #save the result in the matrix
    dat[, i] <- score
  }
  #sum the results for each racial group
  results <- rowSums(dat, na.rm = T)

  results
}
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
#' @export
exposure <- function(group1, group2, totalPop){
  expo <- ifelse(totalPop == 0, 0,
    ( (group1/sum(group1, na.rm=T)) * (group2/totalPop) )
  )
  (expo)
}
