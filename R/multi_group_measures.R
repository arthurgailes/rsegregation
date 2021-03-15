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
  divergence_sanity(rowTotals=rowTotals,sumPercent=sumPercent)
  # if population is character, take the column from the dataframe
   if (is.character(population)){
    popChar <- population
    population <- groupMatrix[[popChar]]
    groupMatrix[popChar] <- NULL
   }
  #process population
  population <- multigroup_population(groupMatrix=groupMatrix, population=population, weights=weights, na.rm=na.rm)

  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0

  # get summary proportions for the full dataset
  largeGroup <- sumProportion(groupMatrix=groupMatrix, population=population,comparison=comparison)
  # check for construction problems
  multigroup_sanity(groupMatrix=groupMatrix,population=population,comparison=comparison)
  # create by-group scores
  prescores <- groupMatrix
  for(column in seq_along(groupMatrix)){
    group <- groupMatrix[[column]]
    # use `population` or `comparison` for large group stats
    group_large <- largeGroup[[column]]
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
multigroup_sanity <- function(groupMatrix, population, comparison){
  if(isTRUE(any(groupMatrix<0))) warning("Negative numbers detected; may skew results")
  # ensure that percentages sum to about 1
  if(isTRUE(any(rowSums(groupMatrix)>1.05))) warning("Some of the provided rows sum to more than 1; check that input values are proportions.")
  if(!is.null(comparison) & length(comparison) != length(groupMatrix)) stop("`comparison` must be the same length the number of columns in `...`")
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
#' \item{"overall_entropy"}{Overall entropy for the summarized dataset. Reports one score for the
#' entire dataset, as when setting `summed`=TRUE.}
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
entropy <- function( ..., population=NA, comparison=NULL, entropy_type = 'entropy', logBase=exp(1),
  scale = FALSE, summed=FALSE, na.rm=TRUE, weights = NA, sumPercent = NA){

  groupMatrix <- data.frame(...)
  # if(nrow(groupMatrix) == 1) return(0) # if a single observation composes a group
  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0

  #process population/weights
  population <- multigroup_population(groupMatrix=groupMatrix, population=population, weights=weights, na.rm=na.rm)

  # check for construction problems
  multigroup_sanity(groupMatrix=groupMatrix,population=population,comparison=comparison)
  # create by-group scores
  prescores <- groupMatrix
  # get summary proportions for the full dataset
  largeGroup <- sumProportion(groupMatrix=groupMatrix, population=population,comparison=comparison)
  # calculate entropy
  for(column in seq_along(groupMatrix)){
    group <- groupMatrix[[column]]
    group_large <- largeGroup[[column]]
    # calculate group, substituting 0 for log(0)
    prescores[[column]] <- ifelse((group <= 0 | group_large <= 0), 0,
      group * log(1 / group, base=logBase) )
  }
  entropy <- rowSums(prescores, na.rm = na.rm)

  if(isTRUE(scale)){
    #scale entropy between zero and 1, where 1 represents log(number of groups)
    entropy <- scale01(entropy, 0, log(length(groupMatrix), base=logBase))
  }

  if(entropy_type %in% c('information_theory','overall_entropy')) {
    # the entropy/diversity of the overall population
    overall_entropy <- sum(sapply(largeGroup, function(x) ifelse(x <= 0, 0, x * log(1 / x, base=logBase) )))
    if (entropy_type=='overall_entropy') return(overall_entropy)
    # sanity check
    if(isTRUE(scale)) stop("scale is not yet compatible with information_theory")
    return(information_theory(entropy=entropy, overall_entropy=overall_entropy, population=population, summed=summed, logBase=logBase))
  }

  # in either entropy_type, the entropy of the larger geography is needed
  if(summed==T) {
    #large population entropy score
    entropySum <- stats::weighted.mean(entropy, population)
    return(entropySum)
  }

  return(entropy)
}
# information theory
information_theory <- function(entropy, overall_entropy, population, summed, logBase){
  # Information index - single observation
  index <- 1 - (entropy/overall_entropy)

  #index score
  if(isTRUE(summed)) index <- sum(population * index)
  if(isTRUE(summed == 'weighted')) index <- (index * population)

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

# convert matrix to percentages or normalize percentages to sum to one
to_percentages <- function(df, rowTotals = NA, na.rm){
  if(isTRUE(is.na(rowTotals))) rowTotals <- rowSums(df, na.rm = na.rm)
  else if(isTRUE(rowTotals == "100%")) rowTotals <- rep(1, nrow(df))
  df <- df/rowTotals
  return(df)
}
# sanity checks for divergence
divergence_sanity <- function(rowTotals, sumPercent){
  if(!is.na(rowTotals) | !is.na(sumPercent)) stop('One of your parameters has been deprecated.')
}
# handle population/weight inputs
multigroup_population <- function(groupMatrix, population, weights=NA, na.rm){
  # throw warning if weights are provided
  if(!isTRUE(is.na(weights))){
    warning('parameter `weights` is deprecated, use `population`')
    population <- weights
  }
  # warning if population is NA; give equal weights
  if(isTRUE(is.na(population))){
    population <- rep(1, nrow(groupMatrix))
    warning("Population parameter not set; assuming equal populations.")
  }
  #remove population NAs
  if(isTRUE(na.rm)) population[which(is.na(population))] <- 0
  # convert population from numbers to weights
  population <- population/sum(population)
  return(population)
}
# summarize group inputs into their full population proportions
sumProportion <- function(groupMatrix, population, comparison){
  if(!is.null(comparison)) largeGroup <- comparison
  else largeGroup <- sapply(groupMatrix, stats::weighted.mean, population)
  return(largeGroup)
}
