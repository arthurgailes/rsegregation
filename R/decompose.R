#' Additively decompose divergence scores by and within groups
#'
#' The Divergence Index is additively decomposable. This function allows for
#' splitting a population into groups of observations and calculating the
#' divergence score within those groups and between those groups.
#'
#' The sum of the scores reported in `decompose_divergence` when setting summed==TRUE
#' should always be equal to the
#'
#' @param dataframe A dataframe composed of numeric/integer columns representing percentages of each
#' population group. All columns are used in the divergence calculation
#' except for those specified in `groupCol` and `popCol`(optional), and no other columns should
#' be included.
#'
#' @param groupCol Name of the column(s) in the dataframe used for grouping.
#' if passing a `grouped_df` to `dataframe`, this parameter is ignored. If using multiple
#' groups, divergence will be aggregated by all unique combinations of all groups, and compared
#' to the total datafame
#'
#' @importFrom stats weighted.mean
#'
#' @param popCol Either NA (default), which sets the population of each row to 1,
#'  or a character string of the column name in `dataframe`.
#'
#' @param weightCol alias for popCol
#'
#' @param output Any of:
#' \describe{
#'  \item{"scores"}{Default. The individual within and between divergence scores for each
#'  row or group, plus the total score.}
#'  \item{"percentage"}{One row for each entry(or group) as in "scores," but scaled so each
#'  observation reports a percentage of the total score that would be reproted with "summed".}
#'  \item{"all"}{The output from `summed`, `weighted`, and `percentage.`}
#'  }
#'
#' @param ... options passed through to `divergence`
#' @note The `divergence` parameters for each group are set to their defaults
#' unless explicitly noted above.
#'
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
#' @note `decompose_divergence` treats the entire dataset its given as the total population,
#' which may not be desirable in some contexts, for example, when trying to return divergence
#' scores across years. In that context, it's helpful to split the dataframe into a list of
#' dataframes and use `decompose_divergence` inside a sapply function.
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
#' @importFrom rlang .data
#'
#' @source Roberto, 2016. "A Decomposable Measure of Segregation and Inequality."
#' @export
decompose_divergence <- function(dataframe, groupCol=NULL, popCol = NA, weightCol = NA,
  output = 'scores', ...){
  #save original dataframe
  dataframe_orig <- dataframe
  if(!is.na(weightCol)& is.na(popCol)) popCol <- weightCol
  if(is.na(popCol)) stop("`popCol` must be specified")
  # convert popCol to weights

  #group the dataframe if groupCol is provided
  if(!is.null(groupCol)) dataframe <- dplyr::group_by(dataframe, dplyr::across(groupCol))
  else if(!dplyr::is.grouped_df(dataframe)) stop("`groupCol` must be specified")
  else groupCol <- dplyr::group_vars(dataframe)

  # rename column to popCol for consistency
  dataframe <- dplyr::rename(dataframe, popCol = {{popCol}})
  # convert popCol to weights
  dataframe$popCol <- dataframe$popCol/sum(dataframe$popCol, na.rm=T)
  # get overall divergence
  divSum <- dplyr::select(dplyr::ungroup(dataframe), -dplyr::all_of(groupCol))
  calcNames <- names(dplyr::select(divSum, -popCol))
  divSum <- rsegregation::divergence(divSum, population='popCol', summed=T,  ...)

  # get "within' divergence for each group
  withinDiv <- dplyr::summarize(dataframe,
    within = rsegregation::divergence(dplyr::across(calcNames),
      population = popCol,summed = T,...), .groups = 'drop'
    )

  # sum population and weights by group
  groupPops <- dplyr::summarize(dataframe, dplyr::across(calcNames,
    ~stats::weighted.mean(.x, popCol, na.rm = T)),
    popCol = sum(popCol, na.rm = T), .groups = 'drop')

  # get divergence "between" group and total
  betweenDiv <- dplyr::transmute(groupPops,
    between = divergence(dplyr::across(calcNames), population = popCol, ...),
    dplyr::across(c(groupCol, popCol)))

  # join within and between together and sum
  result <- dplyr::inner_join(withinDiv, betweenDiv, by = groupCol)
  result$total <- result$within+result$between

  # sanity check that within + between = total divergence
  divSumGroup <- ifelse(sum(result$popCol)==0, 0,
    stats::weighted.mean(result$total, result$popCol, na.rm=T)
  )
  if(round(divSum, 4) != round(divSumGroup, 4)) warning("sum of within and between divergence is not equal to sum of total divergence. Check inputs.")

  # process output parameter
  if(output == 'scores') result <- subset(result, select = -popCol)
  else if(output == 'percentage'){
    result <- dplyr::transmute(result, within = (.data$within*.data$popCol)/divSum,
      between = (.data$between*.data$popCol)/divSum, .data[[groupCol]])
  } else if (output=='all') {
    result <- dplyr::mutate(result, within_pct = (.data$within*.data$popCol)/divSum,
      between_pct = (.data$between*.data$popCol)/divSum)
  } else stop("output parameter is invalid")

  return(result)
}
