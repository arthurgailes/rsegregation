#' Divergence Index
#'
#' Elizabeth Roberto's Divergence index for calculating and decomposing segregation.
#'
#' @param ... Population vectors for every group included in the divergence
#' calculation.
#'
#' @param weights Either a vector of weights summing to one, or the total
#' population for each observation in `...`. Can be any of:
#' \describe{
#'  \item{A numeric vector}{The length of each vector provided in each
#'  entry in `...` (i.e. the column-wise length of `...`). Can be either population
#'  or weights by observation}
#'  \item{`sum`}{Sets `weights` to the rowwise sum of `...`}
#'  \item{`none`}{Weighs each observation evenly (1/length). Note that if `...` is a
#'  set of percentages summing to 1 for each row, `sum` and `none` are equivalent.}
#'  }
#'
#' @param sumPercent The percentage of each group in the larger population (i.e. the
#' population composed of the sum of groups provided in `...`). Can be any of:
#' \describe{
#'  \item{A numeric vector}{Each entry represents the total population of the  with one entry for each group/vector provided in `...`. (i.e.
#'  the rowwise length of `...`).
#'  Note that for this to work correctly, each group must be provided in the same order in
#'  `sumPercent` as in `...` Useful if the sum of `sumPercent` is less than 100%, as `divergence`
#'  will otherwise force}
#'  \item{`weights`}{Default. Uses the value of `weights` to construct total population proportions.
#'  If weights is set to `none`, the total population proportions will be the unweighted
#'  average of the percentages in each observation.}
#'  }
#'
#' @param rowTotals By default, the rowwise sum of `...` is treated as 100% of the population.
#' Setting this parameter to any of the following options chagnes this behavior.
#' \describe{
#'  \item{`NA`}{Default. Forces the rowwise sum of `...` to equal 100% of the
#'  population. Works with either percentages or population totals.}
#'  \item{`100%`}{Uses 1 (100%) as the row total. For this to
#'  work properly, each column provided to `...` must be percentages, not
#'  population totals. Useful if the rowwise sum of percentages is less than 100%; only use if
#'  you know what you are doing.}
#'  \item{`weights`}{Uses the "weights" parameter as the total population.}
#'  \item{A numeric vector}{With length equal to each vector provided in each
#'  entry in `...` (i.e. the column-wise length of `...`). }
#'  }
#'
#' @param summed If TRUE, will return a single summary statistic. (Or one value per group if specifying
#' `dplyr::group_by`.) If FALSE (default), will return a vector equaling the length
#' of the input vectors. If 'weighted' (only for divergence and information theory), returns
#'  a vector as in FALSE, but with pre-weighted values,
#' such that `sum(divergence(..., summed = 'weighted))` is equivalent to
#' `divergence(..., summed = T)`.
#'
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' Used only if `summed` is set to TRUE.
#'
#' @return A single value if summed==TRUE, or a vector equaling the length of the inputs.
#'
#' @examples
#' library(rsegregation)
#' data("bay_race")
#' #return by-observation scores
#' divergence(bay_race$white,bay_race$hispanic,bay_race$asian,
#' bay_race$black, bay_race$all_other)
#'
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
#'   black, all_other, weights = total_pop))
#' }
#'
#' @source Created by Elizabeth Roberto: <https://arxiv.org/abs/1508.01167>
#' @export
divergence <- function(..., weights = 'sum', na.rm=TRUE, summed=FALSE,
  sumPercent = 'weights', rowTotals = NA){

  groupMatrix <- data.frame(...)
  if(nrow(groupMatrix) == 1) return(0) # if a single observation composes a group
  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0

  # keep original weights in case it is a vector of populations

  #deal with weights and sumPercent in separate functions
  if(isTRUE(rowTotals=='weights')) rowTotals <- weights
  weights <- convert_weights(groupMatrix, weights, na.rm = na.rm)
  #convert to percentages if necessary
  groupMatrix <- to_percentages(groupMatrix, rowTotals, na.rm)
  sumPercent <- proc_sumPercent(groupMatrix, sumPercent, weights, na.rm)

  # check for construction problems
  multigroup_sanity(groupMatrix,weights)
  # create by-group scores
  prescores <- groupMatrix
  for(column in seq_along(groupMatrix)){
    group <- groupMatrix[[column]]
    group_large <- sumPercent[[column]]
    # calculate group, substituting 0 for log(0)
    prescores[[column]] <- ifelse(group <= 0 | group_large <= 0, 0,
      group * log(group / group_large) )
  }
  #sum the results for each racial group for divergence score
  results <- rowSums(prescores, na.rm = na.rm)

  # apply weights according to summed parameter
  if(isTRUE(summed)) results <- sum(results * weights, na.rm = na.rm)
  if(isTRUE(summed == 'weighted')) results <- (results * weights)
  return(results)
}
# Sanity checks and warnings for divergence and entropy
multigroup_sanity <- function(df, weights){
  if(isTRUE(any(df<0))) warning("Negative numbers detected; may skew results")
}
#' Theil's Index of Entropy
#'
#' Entropy is used to measure the the extent to which multiple distributions conform to
#' a baseline.
#'
#' @param scale Scale entropy scores from 0-1. Setting scale to TRUE
#' ignores the entropy_type and summed parameters
#'
#' @param entropy_type One of: \describe{
#' \item{"entropy"}{Default. t index in wiki aka entropy score}
#' \item{"information_theory"}{Theil's information theory index}
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
#' @source Theil, Henri. 1972. Statistical Decomposition Analysis.
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Theil_index}
#'
#' @examples
#' library(rsegregation)
#' entropy(bay_race$white,bay_race$hispanic,bay_race$asian,
#' bay_race$black, bay_race$all_other, weights = bay_race$total_pop)
#'
#' @export
entropy <- function( ..., weights = 'sum', sumPercent = 'weights', entropy_type = 'entropy',
  scale = FALSE, summed=FALSE, na.rm=TRUE){

  groupMatrix <- data.frame(...)
  # if(nrow(groupMatrix) == 1) return(0) # if a single observation composes a group
  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0

  #deal with weights and sumPercent in separate functions
  weights <- convert_weights(groupMatrix, weights, na.rm = na.rm)
  #convert to percentages if necessary
  groupMatrix <- to_percentages(groupMatrix, na.rm=na.rm)
  sumPercent <- proc_sumPercent(groupMatrix, sumPercent, weights, na.rm)
  # check for construction problems
  multigroup_sanity(groupMatrix,weights)
  # create by-group scores
  prescores <- groupMatrix
  # calculate entropy
  for(column in seq_along(groupMatrix)){
    group <- groupMatrix[[column]]
    group_large <- sumPercent[[column]]
    # calculate group, substituting 0 for log(0)
    prescores[[column]] <- ifelse((group <= 0 | group_large <= 0), 0,
      group * log(1 / group) )
  }
  entropy <- rowSums(prescores, na.rm = na.rm)
  if(isTRUE(scale)){
    #scale entropy between zero and 1, where 1 represents log(number of groups)
    entropy <- scale01(entropy, 0, log(length(groupMatrix)))
  }

  if(entropy_type == 'information_theory') {
    # sanity check
    if(isTRUE(scale)) stop("scale is not compatible with information_theory")
    return(information_theory(entropy, sumPercent, weights, summed))
  }

  # in either entropy_type, the entropy of the larger geography is needed
  if(summed==T) {
    #large population entropy score
    entropySum <- ifelse(sumPercent <= 0, 0,
      sumPercent * log(1 / sumPercent) )
    entropySum <- sum(entropySum)
    if(isTRUE(scale)) entropySum <- scale01(entropySum, 0, log(length(sumPercent)))
    return(entropySum)
  }

  return(entropy)
}
# information theory
information_theory <- function(entropy, sumPercent, weights, summed){
  #large population entropy score
  entropySum <- ifelse(sumPercent <= 0, 0,
    sumPercent * log(1 / sumPercent) )
  entropySum <- sum(entropySum)
  # Information index - single observation
  index <- 1 - (entropy/entropySum)

  #index score
  if(isTRUE(summed)) index <- sum(weights * index)
  if(isTRUE(summed == 'weighted')) index <- (index * weights)

  return(index)
}
# convert sums into weights if necessarty
convert_weights <- function(df, weights, na.rm){
  #create equal weights
  if(isTRUE(weights == 'none')) weights = rep_len(1, nrow(df))
  #create weights from summed rows
  if(isTRUE(weights == 'sum')) weights = rowSums(df, na.rm = na.rm)
  # else weights should be a given vector, all paths lead here:
  # divide each weight by their sum to make all sum to 1
  sumweight <- sum(weights, na.rm = na.rm)
  weights <- weights/sumweight
  return(weights)
}
# create sumPercent from weights if necessary
proc_sumPercent <- function(df, sumPercent, weights, na.rm){
  # conversion from weights if specified
  if(isTRUE(sumPercent == 'weights')){
    popTotals <- df * weights
    popTotals <- colSums(popTotals, na.rm = na.rm)
    sumPercent <- popTotals/sum(popTotals, na.rm = na.rm)
  }
  return(sumPercent)
}
# convert matrix to percentages or normalize percentages to sum to one
to_percentages <- function(df, rowTotals = NA, na.rm){
  if(isTRUE(is.na(rowTotals))) rowTotals <- rowSums(df, na.rm = na.rm)
  else if(isTRUE(rowTotals == "100%")) rowTotals <- rep(1, nrow(df))
  df <- df/rowTotals
  return(df)
}
