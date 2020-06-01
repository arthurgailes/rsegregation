#' Divergence Index
#'
#' Elizabeth Roberto's Divergence index for calculating and decomposing segregation.
#'
#' @param ... Population vectors for every group included in the divergence
#' calculation.
#'
#' @param weights Only used if `summed` or `weighted` is set to `TRUE`.
#' This can be either a vector of weights summing to one, or the total
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
#' @param sumProp The percentage of each group in the larger population (i.e. the
#' population composed of the sum of groups provided in `...`). Can be any of:
#' \describe{
#'  \item{A numeric vector}{Each entry represents the total population of the  with one entry for each group/vector provided in `...`. (i.e.
#'  the rowwise length of `...`).
#'  Note that for this to work correctly, each group must be provided in the same order in
#'  `groupSums` as in `...`}
#'  \item{`weights`}{Default. Uses the value of `weights` to construct total population proportions.
#'  If weights is set to `none`, the total population proportions will be the unweighted
#'  average of the percentages in each observaiton.}
#'  }
#'
#' @param summed If TRUE, will return a single summary statistic. (Or one value per group if specifying
#' `dplyr::group_by`.) If FALSE, will return a vector equaling the length
#' of the input vectors.
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
  sumProp = 'weights'){

  groupMatrix <- data.frame(...)
  if(nrow(groupMatrix) == 1) return(0) # if a single observation composes a group
  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0

  #deal with weights and sumProp in separate functions
  weights <- convert_weights(groupMatrix, weights, na.rm = na.rm)
  sumProp <- proc_sumProp(groupMatrix, sumProp, weights, na.rm)
  #convert to percentages if necessary
  groupMatrix <- to_percentages(groupMatrix, na.rm)

  # check for construction problems
  multigroup_sanity(groupMatrix,weights)
  # create by-group scores
  prescores <- groupMatrix
  for(column in seq_along(groupMatrix)){
    group <- groupMatrix[[column]]
    group_large <- sumProp[[column]]
    # calculate group, substituting 0 for log(0)
    prescores[[column]] <- ifelse(group <= 0 | group_large <= 0, 0,
      group * log(group / group_large) )
  }
  #sum the results for each racial group for divergence score
  results <- rowSums(prescores, na.rm = na.rm)

  # create total divergence score if selected
  if(isTRUE(summed)) results <- sum(results * weights, na.rm = na.rm)
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
#' @param scaled Scale entropy scores from 0-1. Setting scaled to TRUE
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
#' bay_race$black, weights = bay_race$total_pop)
#'
#' @export
entropy <- function( ..., weights = 'sum', sumProp = 'weights', entropy_type = 'entropy',
  scaled = FALSE, summed=FALSE, na.rm=TRUE){

  groupMatrix <- data.frame(...)
  if(nrow(groupMatrix) == 1) return(0) # if a single observation composes a group
  # remove NAs
  if(isTRUE(na.rm)) groupMatrix[is.na(groupMatrix)] <- 0

  #deal with weights and sumProp in separate functions
  weights <- convert_weights(groupMatrix, weights, na.rm = na.rm)
  sumProp <- proc_sumProp(groupMatrix, sumProp, weights, na.rm)
  #convert to percentages if necessary
  groupMatrix <- to_percentages(groupMatrix, na.rm)
  # check for construction problems
  multigroup_sanity(groupMatrix,weights)
  # create by-group scores
  prescores <- groupMatrix
  # calculate entropy
  for(column in seq_along(groupMatrix)){
    group <- groupMatrix[[column]]
    group_large <- sumProp[[column]]
    # calculate group, substituting 0 for log(0)
    prescores[[column]] <- ifelse(group <= 0 | group_large <= 0, 0,
      group * log(1 / group) )
  }
  entropy <- rowSums(prescores, na.rm = na.rm)
  if(isTRUE(scaled)){
    #scale entropy between zero and 1, where 1 represents log(number of groups)
    entropy <- scale01(entropy, 0, log(length(groupMatrix)))
    # sanity checks
    if(entropy_type != 'entropy' | isTRUE(summed)){
      warning("scaled set to TRUE, ignoring entropy_type and
        summed parameters")
    }
    return(entropy)
  }

  if(entropy_type == 'information_theory') {
    return(information_theory(entropy, sumProp, weights, summed))
  }

  # in either entropy_type, the entropy of the larger geography is needed
  if(summed==T & entropy_type != 'information_theory') {
    #large population entropy score
    entropySum <- ifelse(sumProp <= 0, 0,
      sumProp * log(1 / sumProp) )
    entropySum <- sum(entropySum)
    return(entropySum)
  }

  return(entropy)
}
# information theory
information_theory <- function(entropy, sumProp, weights, summed){
  #large population entropy score
  entropySum <- ifelse(sumProp <= 0, 0,
    sumProp * log(1 / sumProp) )
  entropySum <- sum(sumProp)
  # Information index - single observation
  index <- 1 - (entropy/entropySum)

  #index score
  if(isTRUE(summed)) index <- sum(weights * index)

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
# create sumProp from weights if necessary
proc_sumProp <- function(df, sumProp, weights, na.rm){
  # conversion from weights if specified
  if(isTRUE(sumProp == 'weights')){
    popTotals <- df * weights
    popTotals <- colSums(df, na.rm = na.rm)
    sumProp <- popTotals/sum(popTotals, na.rm = na.rm)
  }
  return(sumProp)
}
# convert matrix to percentages or normalize percentages to sum to one
to_percentages <- function(df, na.rm){
  # test if rows add up to whole numbers
  for(row in rowSums(df, na.rm = T)){
    if(!isTRUE(row %% 1 == 0)) warning("The sum of at least one row in `...` is not a whole number. `divergence` converts all rows in `...` to a percentage of their sum total.")
  }
  df <- df/rowSums(df, na.rm = na.rm)
  return(df)
}
