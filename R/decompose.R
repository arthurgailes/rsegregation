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
#' if passing a `grouped_df` to `dataframe`, this parameter is ignored.
#'
#' @importFrom stats weighted.mean
#'
#' @param weightCol Either NA (default), which sets `weightCol` to the rowwise
#' sum of `dataframe`; or a character string of the column name in `dataframe`
#' that should be used for weightCol.
#'
#' @param output Any of:
#' \describe{
#'  \item{"scores"}{Default. The individual divergence scores for each row or group.}
#'  \item{"weighted"}{One observation per row or group, weighted by the input to the `weightCol`
#'  parameter. The sum of "weighted" scores is equivalent to inputing the "summed".}
#'  \item{"sum"}{Reports one observation of the summed divergence score for the total dataset.}
#'  \item{"percentage"}{One row for each entry(or group) as in "scores," but scaled so each
#'  observation reports a percentage of the total score, as would be reproted with "summed".}
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
#'
#' @export
decompose_divergence <- function(dataframe, groupCol = NULL, weightCol = NA,
  output = 'scores'){
  #save original dataframe
  dataframe_orig <- dataframe
  #group the dataframe if groupCol is provided
  if(!dplyr::is.grouped_df(dataframe)){
    if(is.null(groupCol)) stop('groupCol must be specified')
    # group by the grouping variables
    dataframe <- dplyr::group_by(dataframe, {{groupCol}})
  }
  #save goupnames
  group_var <- dplyr::group_vars(dataframe)
  groupCol <- dplyr::group_cols(dataframe)
  # create weights from rowsums if specified
  if(is.na(weightCol)) {
    dataframe$weightCol = rowSums(
      dplyr::select(dataframe, !dplyr::all_of(group_var)), na.rm=T)
  } else { # if provided as a column, rename column to weightCol
    dataframe <- dplyr::rename(dataframe, weightCol = {{weightCol}})
  }
  # ensure sum of weights == 1 (i.e. convert if weights is provided as raw population stats)
  dataframe$weightCol <- dataframe$weightCol/sum(dataframe$weightCol, na.rm=T)

  # save names of the population variables (i.e. not grouping or weights)
  calcNames <- names(dplyr::select(dataframe, !all_of(group_var),!weightCol))
  # convert dataframe to percentages
  dataframe <- dplyr::mutate(dataframe, dplyr::across({{calcNames}},
    ~(.x/rowSums(dataframe[calcNames]))))

  # get "within' divergence for each group
  withinDiv <- dplyr::summarize(dataframe,
    within = rsegregation::divergence({{calcNames}}, weights = weightCol,
      summed = T), .groups = 'keep'
    )

  # sum population and weights by group
  groupPops <- dplyr::summarize(dataframe, across({{calcNames}},
    ~stats::weighted.mean(.x, weightCol, na.rm = T)),
    weightCol = sum(weightCol, na.rm = T), .groups = 'keep')

  # get divergence "between" group and total
  betweenDiv <- dplyr::transmute(groupPops, {{group_var}}, weightCol,
    between = divergence({{calcNames}}, weights = weightCol))

  # join within and between together
  result <- dplyr::inner_join(withinDiv, betweenDiv, by = group_var)

  # check that within + between = total divergence
  divSum <- divergence(dataframe_orig[calcNames], weights = weightCol, summed = T)
  divSumGroup <- stats::weighted.mean(result$within + result$between,
    result$weightCol, na.rm=T)
  if(round(divSum, 5) != round(divSumGroup, 5)) warning("sum of within and between divergence is not equal to sum of total divergence. Check inputs.")

  # process output parameter
  if(output == 'scores') return(result)
  else if(output == 'weighted') {
    result <- dplyr::transmute(result, within = within*weightCol,
    between = between * weightCol, {{group_var}})
  } else if(output == 'sum') {
    result <- dplyr::summarize(result, dplyr::across(within,between),
      ~stats::weighted.mean(.x, weightCol, na.rm=T))
  } else if(output == 'percentage'){
    result <- dplyr::transmute(result, within = (within*weightCol)/divSum,
      between = (between*weightCol)/divSum, {{group_var}})
  } else stop("output parameter is invalid")

  return(result)
}
