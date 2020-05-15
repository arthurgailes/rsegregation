#' Divergence Index
#'
#' FIX? divergence_pct Calculates the divergence index of segregation
#'
#' @param totalPop A vector for the total population (not group population).
#' Only necessary if the vectors
#' in `...` are percentages, rather than population totals, in which case
#' it is used to reconstruct percentages in the larger geography.
#'
#' @param ... Population vectors for every group included in the divergence
#' calculation. If using percentages instead of population totals, specify
#' `totalPop`
#'
#' @param weighted Return population-weighted divergence scores for each
#' observation. Has no effect if `.sum` is true, as the total divergence
#' score is always the sum of weighted values.
#'
#' @param .sum If TRUE, will return a single summary statistic. (Or one value per group if specifying
#' `dplyr::group_by`.) If FALSE, will return a vector equaling the length
#' of the input vectors.
#'
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' Used only if `.sum` is set to TRUE.
#'
#' @examples
#' data(bay_race)
#' #return by-observation scores
#' divergence(bay_race$white,bay_race$hispanic,bay_race$asian,
#' bay_race$black, bay_race$all_other)
#'
#' \dontrun{
#' # Using dplyr
#' require(dplyr)
#' mutate(bay_race, divergence_score = divergence(white, hispanic,
#'   asian, black, all_other))
#'
#' # divergence alsow works with percentages as long as you have
#' # population totals by observation
#' bay_race %>%
#'   mutate_at(vars(hispanic:all_other), list(~(./total_pop))) %>%
#'   mutate(divergence_score = divergence(white, hispanic, asian,
#'   black, all_other, totalPop = total_pop))
#'
#' # Entering dataframe will cause an error
#' divergence(bay_race[c("white","black","asian","hispanic")])
#' }
#'
#' @source Created by Elizabeth Roberto: <https://arxiv.org/abs/1508.01167>
#' @export
divergence <- function(..., totalPop = NULL, na.rm=TRUE, .sum=FALSE,
  weighted = FALSE){

  groupMatrix <- data.frame(...)
  # remove NAs
  if(na.rm == TRUE) groupMatrix[is.na(groupMatrix)] <- 0
  #If `totalPop` is not provided, create from the sum of groups and convert
  # totals to percentages
  if(is.null(totalPop)) {
    totalPop <- rowSums(groupMatrix, na.rm = na.rm)
    # for observations with zero sum, create a dummy denomintor to prevent
    # NaN results
    denominator <- ifelse(totalPop == 0, 0.1, totalPop)
    groupMatrix <- groupMatrix / denominator
  }
  # check for construction problems
  divergence_sanity(groupMatrix,totalPop)
  # create by-group scores
  prescores <- sapply(groupMatrix, function(group){
    # create overall group proportion in sum of observations
    group_bigGeo <- sum(group*totalPop, na.rm=na.rm) / sum(totalPop, na.rm=na.rm)
    # calculate group, substituting 0 for log(0)
    score <- ifelse(group <= 0 | group_bigGeo <= 0, 0,
      group * log(group / group_bigGeo) )
    return(score)
  })
  #sum the results for each racial group for divergence score
  results <- rowSums(prescores, na.rm = na.rm)

  # create total divergence score if selected
  if(weighted == TRUE & .sum == FALSE) results <- results*totalPop/sum(totalPop, na.rm = na.rm)
  if(.sum==TRUE) results <- sum(results * totalPop / sum(totalPop, na.rm = na.rm), na.rm = na.rm)
  return(results)
}
#' Sanity checks and warnings for divergence
divergence_sanity <- function(df, totalPop){
  if(any(df<0)) warning("Negative numbers detected; may skew results")
  if(any(df>1)) warning("Percentages greater than 100% detected; is `totalPop` specified correctly?")
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
