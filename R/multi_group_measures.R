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
#' @param .sum If TRUE, will return a single summary statistic. (Or one value per group if specifying
#' `dplyr::group_by`.) If FALSE, will return a vector equaling the length
#' of the input vectors.
#'
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' Used only if `.sum` is set to TRUE.
#'
#' @examples
#' divergence(bay_race$white,bay_race$hispanic,bay_race$asian,
#' bay_race$black, totalPop = bay_race$total_pop)
#'
#' \dontrun{
#' # Entering dataframe will cause an error
#' divergence(bay_race[c("white","black","asian","hispanic")])
#' }
#'
#' @source Created by Elizabeth Roberto: <https://arxiv.org/abs/1508.01167>
#' @export
divergence <- function(..., totalPop = NULL, na.rm=TRUE, .sum=FALSE){

  groups <- list(...)

  # convert list of vectors into DF, then convert to percentages
  raceMatrix <- as.data.frame(do.call(cbind, groups))
  raceMatrix[is.na(raceMatrix)] <- 0
  #If total popuation is not provided, create from the sum of groups
  if(is.null(totalPop)) totalPop <- apply(raceMatrix, 1, sum)
  raceMatrix <- raceMatrix / totalPop

  # Create empty matrix with one column for each race
  dat <- matrix(nrow = nrow(raceMatrix), ncol = length(groups))
  #each item in ... should be a matrix of race totals
  i = 0
  for(race in groups){
    # create race proportion
    race_bigGeo <- sum(race, na.rm=na.rm) / sum(totalPop, na.rm=na.rm)
    race <- ifelse(totalPop == 0, 0, race / totalPop)
    i = i + 1
    score <- ifelse(race <= 0 | race_bigGeo <= 0, 0,
      race * log(race / race_bigGeo) )
    #save the result in the matrix
    dat[, i] <- score
  }
  #sum the results for each racial group
  results <- rowSums(dat, na.rm = na.rm)

  # create summary measure
  if(.sum==TRUE) results <- sum(results * totalPop / sum(totalPop))
  return(results)
}
#' Theil's Index of Entropy
#'
#' Entropy is used to measure the the extent to which multiple distributions conform to
#' a baseline. In standard entropy (`entropy()`), the baseline is constant evenness. In
#' Theil's \emph{T} Index
#'
#' @param scaled Scale entropy scores from 0-1. Setting scaled to TRUE
#' ignores the entropy_type and .sum parameters
#'
#' @param entropy_type One of: \describe{
#' \item{score}{t index in wiki aka entropy score}
#' \item{index}{theil's h, akal entropy index, aka Theil's information theory index}
#' }
#'
#' @inheritParams divergence
#'
#' @details \describe{
#'  \item{`entropy`}{Entropy score (Ei). \eqn{E_{i} = \Sigma (X_{im} * ln(1/X_{im})}{Ei = \Sigma (Xim \* ln(1/X_{im}))}
#'  where Xim is the
#'  proportion of racial group within the geography. }
#'  \item{`entropy_score`}{Calculates the value of H (entropy index) for
#'  large-scale geography. }
#'  }
#'
#' @return A single value if .sum==TRUE, or a vector equaling the length of the inputs. Note that if
#' `entropy_type` == "index", and .sum is FALSE, then the returned vector will be entropy index, unweighted by
#'  population
#'
#'
#' @source Theil, Henri. 1972. Statistical Decomposition Analysis.
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Theil_index}
#'
#' #' @examples
#' entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
#' bay_race$black, totalPop = bay_race$total_pop)
#' @export
entropy <- function( ..., totalPop = NULL, entropy_type = 'index',
  scaled = FALSE, .sum=TRUE, na.rm=TRUE){

  #each item in ... should be a vector of race populations
  groups <- list(...)
  #calculate basic entropy/diversity score
  entropy <- raw_entropy(groups, totalPop=totalPop,
    scaled=scaled, .sum=.sum, na.rm=na.rm)


  if(scaled == TRUE){
    #scale entropy between zero and 1, where 1 represents log(number of groups)
    entropy <- scale01(entropy, 0, log(length(groups)))
    # sanity checks
    if(entropy_type == 'index' | .sum == TRUE){
      warning("scaled set to TRUE, ignoring entropy_type and
        .sum parameters")
    }
    return(entropy)
  }

  # in either entropy_type, the entropy of the larger geography is needed
  if(.sum==T) {
    #sum the groups and total population (if provided)
    sumgroups <- lapply(groups, sum, na.rm=na.rm)
    sumtot <- ifelse(is.null(totalPop), NULL, sum(totalPop, na.rm=na.rm))
    # calculate entropy on this basis
    entropy_large <- raw_entropy(sumgroups, totalPop=sumtot,
      scaled=scaled, .sum=.sum, na.rm=na.rm)
  }
  if(entropy_type == 'score') {
    if(.sum==T) return(entropy_large)
    else return(entropy)
  } else if (entropy_type == 'index'){
    # Theil's Hi
    entropy_index <- (entropy_large - entropy) / entropy_large
    # Theil's H
    if(.sum==T) entropy_index <- sum(entropy_index * (totalPop / sum(totalPop, na.rm=na.rm)))
    return(entropy_index)
  } else (stop("entropy_type must be either 'score' or 'index'"))
}
# calculate entropy/diversity
raw_entropy <- function(groups, totalPop = NULL,
  scaled = FALSE, .sum=TRUE, na.rm=TRUE){

  # convert list of vectors into DF, then convert to percentages
  raceMatrix <- as.data.frame(do.call(cbind, groups))
  raceMatrix[is.na(raceMatrix)] <- 0
  #If total popuation is not provided, create from the sum of groups
  if(is.null(totalPop)) totalPop <- apply(raceMatrix, 1, sum)
  raceMatrix <- raceMatrix / totalPop
  raceCols <- (colnames(raceMatrix))

  #create empty matrix the length of the matrix
  dat <- matrix(nrow = nrow(raceMatrix), ncol = length(groups))
  i = 0
  for(race in groups){
    # create race proportion
    race <- ifelse(totalPop == 0, 0, race / totalPop)
    i = i + 1
    score <- ifelse(race <= 0, 0,
      race * log(1/race) )
    dat[, i] <- score
  }
  #sum the results for each racial group
  entropy <- rowSums(dat, na.rm = T)

}
