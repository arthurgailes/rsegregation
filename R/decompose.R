#' Additively decompose divergence scores by and within groups
#'
#' The Divergence Index is additively decomposable. This function allows for
#' splitting a population into groups of observations and calculating the
#' divergence score within those groups and between those groups.
#'
#' The sum of the scores reported in `decompose_divergence` when setting summed==TRUE
#' should always be equal to the
#'
#' @param dataframe A dataframe composed of numeric/integer columns, with the exception
#' of a column named in `groupCol`. All columns are used in the divergence calculation
#' except for those specified in `groupCol` and `weights`(optional)
#'
#' @param groupCol Name of the column in the dataframe used for grouping or a vector of groups
#' equalength the number of rows in dataframe. Ignored if passing a grouped_df
#' to `dataframe`.
#'
#' @inheritParams divergence
#'
#' @param weights Any of:
#' \describe{
#'  * `sum`(default) Sets `weights` to the rowwise sum of `dataframe`
#'  * A character string of the column to use as the column of weights in
#'  `dataframe`.
#'  * A numeric vector the length of the number of rows of `dataframe`
#'  }
#'
#' @param output Not yet implemented.
#' @param ... passed to \code{\link{divergence}}. Not yet implemented
#'
#' @note The `divergence` parameters for each group are set to their defaults
#' unless explicitly noted above.
#'
#' @return A dataframe with one row if `summed == TRUE` or else one row for each
#' list or group in `dataframe`.
#'
#' The dataframe will have two columns: 'within_divergence', equivalent to
#'  `divergence()` for each dataframe or group in `dataframe`; and
#'  'between_divergence': the divergence score of each group's demographics compared
#'  to the full population. The sum of `decompose_divergence(...,summed = T)` should
#'  equal the result of `divergence(...,summed = T)`
#'
#' @source Roberto, 2016. "A Decomposable Measure of Segregation and Inequality."
#'
#' @export
decompose_divergence <- function(dataframe, groupCol = class(dataframe), weights = 'sum',
  output = 'scores', ...){
  if('grouped_df' %in% class(dataframe)){
    #for dplyr, extraxt grouping variable
    group_var = dplyr::group_vars(dataframe)
    groupCol = dplyr::group_indicies(dataframe)
    dataframe = as.data.frame(dataframe)
  }
  # if a column name is provided for groupCol
  else if(length(groupCol) == 1){
    #save name
    group_var <- groupCol
    # collect columns and subset
    groupCol <- dataframe[group_var]
    dataframe <- dataframe[, !colnames(dataframe) %in% group_var, drop=F]
  }


  # create weights from rowsums if specified
  if(isTRUE(weights == 'sum')) weightCol = rowSums(dataframe, na.rm=T)
  # if provided as a vector, add to dataframe
  else if(length(weights) == nrow(dataframe)) weightCol <- 'weights'
  # if provided as a column
  else {
    weightCol <- dataframe[[weights]]
    dataframe <- subset(dataframe, select = -weights)
  }

  #save original dataframe and convert to percentages
  dataframe_orig <- dataframe
  dataframe <- as.data.frame(sapply(names(dataframe), function(col){
    dataframe[col] <- dataframe[[col]]/rowSums(dataframe_orig, na.rm = T)
  }))
  #add weights as column so the grouping indexes it
  dataframe['weights'] <- weightCol

  # get "within' divergence for each group
  withinDiv <- by(dataframe, groupCol, function(group){
    divergence(subset(group, select = -weights), weights = group$weights,
      summed = T)
  })

  # get population means
  totalPop <- sapply(subset(dataframe, select = -weights), weighted.mean,
    dataframe$weights, na.rm = T)
  # summarise population by group
  groupPops <- by(dataframe, groupCol, function(group){
    #get weighted mean by column
    groupSums <- as.data.frame(sapply(subset(group, select = -weights),
      weighted.mean, group$weights, na.rm = T))
    #add sum of weights
    groupSums$weights <- sum(group$weights, na.rm = T)
    return(groupSums)
  })
  groupPops <- do.call(rbind, groupPops)
  # get divergence "between" group and total
  betweenDiv <- divergence(subset(groupPops, select = -weights),
    weights = groupPops$weights)

  # check that within + between = total divergenc
  divSum <- divergence(dataframe_orig, weights = weightCol, summed = T)
  divSumGroup <- weighted.mean(withinDiv + betweenDiv, groupPops$weights, na.rm=T)
  if(round(divSum, 5) == round(divSumGroup, 5)) warning("sum of within and between divergence is not equal to sum of total divergence. Check inputs.")

  #return a dataframe with within, between, and weights
  return(data.frame(within = withinDiv, between = betweenDiv,
    weights = groupPops$weights))

}
