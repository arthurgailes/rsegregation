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
#' @param groupCol Name of the column(s) in the dataframe used for grouping.
#' if passing a `grouped_df` to `dataframe`, this parameter is ignored. If using multiple
#' groups, divergence will be aggregated by all unique combinations of all groups, and compared
#' to the total datafame.
#'
#' @importFrom stats weighted.mean
#'
#' @param weightCol Either NA (default), which sets `weightCol` to the rowwise
#' sum of `dataframe`; or a character string of the column name in `dataframe`
#' that should be used for weightCol.
#'
#' @param output Any of:
#' \describe{
#'  \item{"scores"}{Default. The individual within and between divergence scores for each
#'  row or group, and a column of weights to use if summing them both across the entire
#'  dataset.}
#'  \item{"weighted"}{One observation per row or group, weighted by the input to the `weightCol`
#'  parameter. The sum of "weighted" scores is equivalent to the output of "summed".}
#'  \item{"sum"}{Reports one observation of the summed divergence score for the total dataset.}
#'  \item{"percentage"}{One row for each entry(or group) as in "scores," but scaled so each
#'  observation reports a percentage of the total score that would be reproted with "summed".}
#'  \item{"scaled"}{Not yet implemented. Re-scales divergence scores the divergence index to
#'  have a range  of 0 to 1 by dividing by its maximum value for a given population. See details.}
#'  }
#' @note The `divergence` parameters for each group are set to their defaults
#' unless explicitly noted above.
#'
#' @details
#' Deomposing the divergence index allows users to simultatneously examine the segregation within
#' and between groups of a large geography. Furthermore, users can assess the percentage of
#' segregation coming from each group.
#'
#' The `output` paramater "scaled" transforms the divergence index
#' it from an absolute to a relative measure of inequality and segregation, and negates
#' several of its desirable properties, including aggregation equivalence and independence.
#'  (See Roberto, 2016)
#'
#' @return A dataframe as specified by the `output` parameter.
#'
#' The dataframe will have three columns: 'within_divergence', equivalent to
#'  `divergence()` for each dataframe or group in `dataframe`;
#'  'between_divergence', the divergence score of each group's demographics compared
#'  to the full population; and `weightCol`, the sum of the weights for each group.
#'  The sum of `decompose_divergence(...,summed = T)` should
#'  equal the result of `divergence(...,summed = T)`
#'
#' @source Roberto, 2016. "A Decomposable Measure of Segregation and Inequality."
#' @export
decompose_divergence <- function(dataframe, groupCol = NULL, weightCol = NA,
  output = 'scores'){
  #save original dataframe
  dataframe_orig <- dataframe
  #group the dataframe if groupCol is provided
  if(!dplyr::is.grouped_df(dataframe) & is.null(groupCol)) stop('groupCol must be specified')
  else if(dplyr::is.grouped_df(dataframe)){
    #extract grouping vars and ungroup
    groupCol <- dplyr::group_vars(dataframe)
    dataframe <- dplyr::ungroup(dataframe)
  }
  # create weights from rowsums if specified
  if(is.na(weightCol)) {
    dataframe$weightCol = rowSums(
      dplyr::select(dataframe, !dplyr::all_of(groupCol)), na.rm=T)
  } else { # if provided as a column, rename column to weightCol
    dataframe <- dplyr::rename(dataframe, 'weightCol' = dplyr::all_of(weightCol))
    rm(weightCol)
  }
  # ensure sum of weights == 1 (convert to % if weights is provided as raw population stats)
  dataframe$weightCol <- dataframe$weightCol/sum(dataframe$weightCol, na.rm=T)

  # save names of the population variables (i.e. not grouping or weights)
  calcNames <- names(dplyr::select(dataframe, !dplyr::all_of(groupCol), -'weightCol'))

  # convert dataframe to percentages
  dataframe <- dplyr::mutate(dataframe, dplyr::across(calcNames,
    ~(.x/rowSums(dataframe[calcNames]))))

  # group by the grouping variables
  dataframe <- dplyr::group_by(dataframe, dplyr::across(groupCol))

  # get "within' divergence for each group
  withinDiv <- dplyr::summarize(dataframe,
    within = rsegregation::divergence(dplyr::across(calcNames),
      weights = weightCol,summed = T), .groups = 'drop'
    )

  # sum population and weights by group
  groupPops <- dplyr::summarize(dataframe, dplyr::across(calcNames,
    ~stats::weighted.mean(.x, weightCol, na.rm = T)),
    weightCol = sum(weightCol, na.rm = T), .groups = 'drop')

  # get divergence "between" group and total
  betweenDiv <- dplyr::transmute(groupPops,
    between = divergence(dplyr::across(calcNames), weights = weightCol),
    dplyr::across(c(groupCol, weightCol)))

  # join within and between together
  result <- dplyr::inner_join(withinDiv, betweenDiv, by = groupCol)

  # sanity check that within + between = total divergence
  divSum <- divergence(dataframe_orig[calcNames], weights = dataframe$weightCol, summed = T)
  divSumGroup <- ifelse(sum(result$weightCol)==0, 0,
    stats::weighted.mean(result$within + result$between, result$weightCol, na.rm=T)
  )
  if(round(divSum, 5) != round(divSumGroup, 5)) warning("sum of within and between divergence is not equal to sum of total divergence. Check inputs.")

  # process output parameter
  if(output == 'scores') return(result)
  else if(output == 'weighted') {
    result <- data.frame(within = result$within*result$weightCol,
      between = result$between * result$weightCol, result[groupCol])
  } else if(output == 'sum') {
    result <- dplyr::summarize(result, dplyr::across(c('within','between'),
      ~stats::weighted.mean(.x, weightCol, na.rm=T)))
  } else if(output == 'percentage'){
    result <- data.frame(within = (result$within*result$weightCol)/divSum,
      between = (result$between*result$weightCol)/divSum, result[groupCol])
  } else stop("output parameter is invalid")

  return(result)
}
