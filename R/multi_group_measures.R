#' Divergence Index
#'
#' @param totalPop Depreciated. Use `weights`.
#'
#' @param ... Population vectors for every group included in the divergence
#' calculation. If using percentages instead of population totals, specify
#' `totalPop`
#'
#' @param weights Only used if `summed` or `weighted` is set to `TRUE`.
#' This can be either a vector of weights summing to one, or the total
#' population for each observation in `...`. Can be any of:
#' \describe{
#'  \item{A numeric or integer vector the length of each vector provided in each
#'  entry in `...`. (i.e. the column-wise length of ...)}
#'  \item{`sum`}{Sets `weights` to the rowwise sum of `...`}
#'  \item{`none`}{Weighs each observation evenly (1/length). Note that if `...` is a
#'  set of percentages summing to 1 for each row, `sum` and `none` are equivalent.}
#'  }
#'
#' @param totalPop The percentage of each group in the larger population (i.e. the
#' population composed of the sum of groups provided in `...`). Can be any of:
#' \describe{
#'  \item{A numeric vector with one entry for each group/vector provided in `...`. (i.e.
#'  the rowwise length of `...`)
#'  Note that for this to work correctly, each group must be provided in the same order in
#'  `groupSums` as in `...`}
#'  \item{`weights`}{Default. Uses the value of `weights` to construct total population proportions.
#'  If weights is set to `none`, the total population proportions will be the unweighted
#'  average of the percentages in each observaiton.}
#'  }
#'
#' @param weighted Return population-weighted divergence scores for each
#' observation. Has no effect if `summed` is true, as the total divergence
#' score is always the sum of weighted values.
#'
#' @param summed If TRUE, will return a single summary statistic. (Or one value per group if specifying
#' `dplyr::group_by`.) If FALSE, will return a vector equaling the length
#' of the input vectors.
#'
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' Used only if `summed` is set to TRUE.
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
#'
#' @source Created by Elizabeth Roberto: <https://arxiv.org/abs/1508.01167>
#' @export
divergence <- function(..., weights = NULL, na.rm=TRUE, summed=FALSE,
  weighted = FALSE, totalPop = NULL){

  groupMatrix <- data.frame(...)
  if(nrow(groupMatrix) == 1) return(0) # if a sinlge observation composes a group
  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0
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
  if(isTRUE(weighted) & isTRUE(summed)) results <- results*totalPop/sum(totalPop, na.rm = na.rm)
  if(isTRUE(summed)) results <- sum(results * totalPop / sum(totalPop, na.rm = na.rm), na.rm = na.rm)
  return(results)
}
# Sanity checks and warnings for divergence
divergence_sanity <- function(df, totalPop){
  if(isTRUE(any(df<0))) warning("Negative numbers detected; may skew results")
  if(isTRUE(any(df>1))) warning("Percentages greater than 100% detected; is `totalPop` specified correctly?")
  if(isTRUE(any(totalPop < 1))) warning("Either totalPop is specified incorrectly, or some percentages do not add to 100%")
}
#' Theil's Index of Entropy
#'
#' Entropy is used to measure the the extent to which multiple distributions conform to
#' a baseline. In standard entropy (`entropy()`), the baseline is constant evenness. In
#' Theil's \emph{T} Index
#'
#' @param scaled Scale entropy scores from 0-1. Setting scaled to TRUE
#' ignores the entropy_type and summed parameters
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
#' @return A single value if summed==TRUE, or a vector equaling the length of the inputs. Note that if
#' `entropy_type` == "index", and summed is FALSE, then the returned vector will be entropy index, unweighted by
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
entropy <- function( ..., weights = totalPop = NULL, entropy_type = 'index',
  scaled = FALSE, summed=TRUE, na.rm=TRUE){

  #each item in ... should be a vector of race populations
  groupMatrix <- data.frame(...)

  if(nrow(groupMatrix) == 1) return(0) # if a sinlge observation composes a group
  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0
  #If `totalPop` is not provided, create from the sum of groups and convert
  # totals to percentages
  if(is.null(totalPop)) {
    totalPop <- rowSums(groupMatrix, na.rm = na.rm)
    # for observations with zero sum, create a dummy denomintor to prevent
    # NaN results
    denominator <- ifelse(totalPop == 0, 0.1, totalPop)
    groupMatrix <- groupMatrix / denominator
  }

  # create by-group scores
  prescores <- sapply(groupMatrix, function(group){
    # create overall group proportion in sum of observations
    group_bigGeo <- sum(group*totalPop, na.rm=na.rm) / sum(totalPop, na.rm=na.rm)
    # calculate group, substituting 0 for log(0)
    score <- ifelse(group <= 0 | group_bigGeo <= 0, 0,
      group * log(1 / group) )
    return(score)
  })

  #sum the results for each racial group for Entropy score Ei.
  entropy <- rowSums(prescores, na.rm = na.rm)

  if(entropy_type == 'index'){

  }


  if(isTRUE(scaled)){
    #scale entropy between zero and 1, where 1 represents log(number of groups)
    entropy <- scale01(entropy, 0, log(length(groups)))
    # sanity checks
    if(entropy_type == 'index' | isTRUE(summed)){
      warning("scaled set to TRUE, ignoring entropy_type and
        summed parameters")
    }
    return(entropy)
  }

  # in either entropy_type, the entropy of the larger geography is needed
  if(summed==T) {
    #sum the groups and total population (if provided)
    sumgroups <- lapply(groups, sum, na.rm=na.rm)
    sumtot <- ifelse(is.null(totalPop), NULL, sum(totalPop, na.rm=na.rm))
    # calculate entropy on this basis
    entropy_large <- raw_entropy(sumgroups, totalPop=sumtot,
      scaled=scaled, summed=summed, na.rm=na.rm)
  }
  if(entropy_type == 'score') {
    if(summed==T) return(entropy_large)
    else return(entropy)
  } else if (entropy_type == 'index'){
    # Theil's Hi
    entropy_index <- (entropy_large - entropy) / entropy_large
    # Theil's H
    if(summed==T) entropy_index <- sum(entropy_index * (totalPop / sum(totalPop, na.rm=na.rm)))
    return(entropy_index)
  } else (stop("entropy_type must be either 'score' or 'index'"))
}

# convert sums into weights if necessarty
convert_weights <- function(df, weights, na.rm){
  if(is.null(weights))
  if(!class(weights) %in% c('numeric','integer')) stop("weights must be class numeric or integer")
  sumweight <- sum(weights, na.rm = na.rm)
  weights <- weights/sumweight
  return(weight)
}
