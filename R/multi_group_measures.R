#' Divergence Index
#'
#' Elizabeth Roberto's Divergence index for calculating and decomposing segregation.
#'
#' @param ... Vectors containing proportions of each group, or a dataframe containing only those vectors.
#'
#' @param population A vector of population totals for each row, or weights summing to a total of
#' one. If NA, will assume all populations/weights are equal. Set to 1 to silence warning.
#' If a string, will use the string as the named column of the dataframe provided in ...
#'
#' @param comparison A vector of percentages that must equal the length of the
#' number of vectors or columns in `...`, representing the percentages of each
#' group in the larger (comparison) geography.
#'
#' @param weights deprecated, use population.
#' @param rowTotals,sumPercent deprecated, will throw error.
#'
#' @param summed If TRUE, will return a single summary statistic. (Or one value per group if specifying
#' `dplyr::group_by`.) If FALSE (default), will return a vector equaling the length
#' of the input vectors. If 'weighted' (only for divergence and information theory), returns
#'  a vector as in FALSE, but with pre-weighted values,
#' such that `sum(divergence(..., summed = 'weighted))` is equivalent to
#' `divergence(..., summed = T)`.
#'
#' @param logBase Specify the base for the logathirm used in the equation. Natural logarithm by default.
#'
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' Used only if `summed` is set to TRUE.
#'
#' @details The demographics of each observation are compared to that of their
#'  larger geography, which is inferred from the combination of the percentages in
#'  `...` and the population totals in `population`, or can be directly provided with
#'  `comparison`.
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
#'
#' @source Created by Elizabeth Roberto: <https://arxiv.org/abs/1508.01167>
#' @export
divergence <- function(..., population=NA, na.rm=TRUE, summed=FALSE, logBase=exp(1),
  comparison=NULL, sumPercent = NA, weights = NA, rowTotals = NA){
  groupMatrix <- data.frame(...)
  #sanity checks
  divergence_sanity(rowTotals=rowTotals,sumPercent=sumPercent,comparison=comparison,
    groupMatrix=groupMatrix)
  if(!isTRUE(is.na(weights))){
    warning('parameter `weights` is deprecated, use `population`')
    population <- weights
  }
  # process population inputs if not actual population/weights
  if(isTRUE(is.na(population))){
    population <- 1
    warning("Population parameter not set; assuming equal populations.")
  } else if (is.character(population)){
    popChar <- population
    population <- groupMatrix[popChar]
    groupMatrix[popChar] <- NULL
  }
  if(isTRUE(any(rowSums(groupMatrix)>1.05))) warning("Some of the provided rows sum to more than 1; check input values.")
  # if(nrow(groupMatrix) == 1) return(0) # if a single observation composes a group
  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0

  # check for construction problems
  multigroup_sanity(groupMatrix,population)
  # create by-group scores
  prescores <- groupMatrix
  for(column in seq_along(groupMatrix)){
    group <- groupMatrix[[column]]
    # use `population` or `comparison` for large group stats
    group_large <- ifelse(is.null(comparison),
      stats::weighted.mean(group, population), comparison[column])
    # calculate group, substituting 0 for log(0)
    prescores[[column]] <- ifelse(group <= 0 | group_large <= 0, 0,
      group * log(group / group_large, base=logBase) )
  }
  #sum the results for each racial group for divergence score
  results <- rowSums(prescores, na.rm = na.rm)

  # apply weights according to summed parameter
  if(isTRUE(summed)) results <- stats::weighted.mean(results, population, na.rm = na.rm)
  if(isTRUE(summed == 'weighted')) results <- (results * population)/sum(population, na.rm=na.rm)
  return(results)
}
# Sanity checks and warnings for divergence and entropy
multigroup_sanity <- function(df, population){
  if(isTRUE(any(df<0))) warning("Negative numbers detected; may skew results")
}
#' Theil's Index of Entropy
#'
#' Entropy is used to measure the the extent to which multiple distributions conform to
#' a baseline.
#'
#' @param scale Scale entropy scores from 0-1. Setting scale to TRUE
#' ignores the entropy_type (set to "entropy") and summed parameters.
#'
#' @param entropy_type One of: \describe{
#' \item{"entropy"}{Default.
#'  \eqn{E_{i} = \Sigma (X_{im} * ln(1/X_{im})}{Ei = \Sigma (Xim \* ln(1/X_{im}))}
#'  where Xim is the proportion of racial group within the geography i.}
#' \item{"information_theory"}{Theil's information theory index}
#' }
#'
#' @inheritParams divergence
#'
#'
#' @return A single value if summed==TRUE, or a vector equaling the length of the inputs. Note that if
#' `entropy_type` == "index", and summed is FALSE, then the returned vector will be entropy index, unweighted by
#'  population
#'
#' @source Theil, Henri. 1972. Statistical Decomposition Analysis.
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Generalized_entropy_index}
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
# sanity checks for divergence
divergence_sanity <- function(...){
  list2env(list(...), envir = environment())
  if(!is.na(rowTotals) | !is.na(sumPercent)) stop('One of your parameters has been deprecated.')
  if(!is.null(comparison) & length(comparison) != length(groupMatrix)) stop("`comparison` must be the same length the number of columns in `...`")
}
