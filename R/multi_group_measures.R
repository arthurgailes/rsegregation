#' Divergence Index
#'
#' Calculates the divergence index of segregation
#'
#' @param totalPop The total population of the geography (e.g. Census
#' tract) being analyzed. If not specified, deaults to the sum of the
#' populations provided in `...`
#'
#' @param ... columns to be included in the calculation of the index.
#'
#' @param .sum If TRUE, will return one value. (Or one value per group if specifying
#' `dplyr::group_by`.) If FALSE, will return a vector equaling the length
#' of the input vectors.
#'
#' @examples
#' divergence(alameda_wide$white,alameda_wide$hispanic,alameda_wide$asian,
#' alameda_wide$black, totalPop = alameda_wide$total_pop)
#'
#' \dontrun{
#' # Entering dataframe will cause an error
#' divergence(alameda_wide[c("white","black","asian","hispanic")])
#' }
#'
#' @param na.rm logical. Should missing values (including NaN) be removed?
#' @source Created by Elizabeth Roberto: <https://arxiv.org/abs/1508.01167>
#' @export
divergence <- function(..., totalPop = NULL, na.rm=TRUE, .sum=FALSE){

  races <- list(...)

  # convert list of vectors into DF, then convert to percentages
  raceMatrix <- as.data.frame(do.call(cbind, races))
  raceMatrix[is.na(raceMatrix)] <- 0
  #If total popuation is not provided, create from the sum of races
  if(is.null(totalPop)) totalPop <- apply(raceMatrix, 1, sum)
  raceMatrix <- raceMatrix / totalPop

  # Create empty matrix with one column for each race
  dat <- matrix(nrow = nrow(raceMatrix), ncol = length(races))
  #each item in ... should be a matrix of race totals
  i = 0
  for(race in races){
    # create race proportion
    race_bigGeo <- sum(race, na.rm=TRUE) / sum(totalPop, na.rm=TRUE)
    race <- ifelse(totalPop == 0, 0, race / totalPop)
    i = i + 1
    score <- ifelse(race <= 0 | race_bigGeo <= 0, 0,
      race * log(race / race_bigGeo) )
    #save the result in the matrix
    dat[, i] <- score
  }
  #sum the results for each racial group
  results <- rowSums(dat, na.rm = T)

  results
}
#' Theil's Index of Entropy
#'
#' Entropy is used to measure the the extent to which multiple distributions conform to
#' a baseline. In standard entropy (`entropy()`), the baseline is constant evenness. In
#' Theil's \emph{T} Index
#'
#' @param scaled Scale entropy scores from 0-1. See sources
#'
#' @param thresholds Returns scores in from 1-3 (low-high diversity), based on the
#'  following criteria: \describe{
#'  \item{1}{Scaled entropy values less than or equal to 0.3707 or one group constitutes
#'  more than 80 percent of the population}
#'  \item{2}{All observations not coded as 1 or 3}
#'  \item{3}{Scaled entropy greater than 0.7414 AND no group constituting more than
#'  45 percent of the population}
#'  } See sources.
#'
#' @param entropy_index Thiel's \emph{T} index
#'
#' @param entropy_smallGeo,entropy_bigGeo The small (e.g. tract, row) and large
#'  (e.g. county, group) entries.
#'
#' @inheritParams divergence
#'
#' @details \describe{
#'  \item{`entropy`}{Entropy score (Ei). \deqn{Ei = \Sigma (X_{im} * ln(1/X_{im})}{Ei = \Sigma (Xim \* ln(1/Xim))}
#'  where Xim is the
#'  proportion of racial group within the geography. }
#'  \item{`entropy_score`}{Calculates the value of H (entropy index) for
#'  large-scale geography. }
#'  }
#'
#' @source Scale and threshold methodology from Holloway et al (2011):
#' \url{https://www.tandfonline.com/doi/abs/10.1080/00330124.2011.585080}
#'
#' @source Theil, Henri. 1972. Statistical Decomposition Analysis.
#'
#' @seealso \url{https://en.wikipedia.org/wiki/Theil_index}
#' @name entropy
NULL
#' @rdname entropy
entropy <- function( ..., totalPop = NULL, scaled = FALSE, thresholds = FALSE){
  #each item in ... should be a vector of race populations
  races <- list(...)

  # convert list of vectors into DF, then convert to percentages
  raceMatrix <- as.data.frame(do.call(cbind, races))
  raceMatrix[is.na(raceMatrix)] <- 0
  #If total popuation is not provided, create from the sum of races
  if(is.null(totalPop)) totalPop <- apply(raceMatrix, 1, sum)
  raceMatrix <- raceMatrix / totalPop
  raceCols <- (colnames(raceMatrix))

  #create empty matrix the length of the matrix
  dat <- matrix(nrow = nrow(raceMatrix), ncol = length(races))
  i = 0
  for(race in races){
    # create race proportion
    race <- ifelse(totalPop == 0, 0, race / totalPop)
    i = i + 1
    score <- ifelse(race <= 0, 0,
      race * log(1/race) )
    dat[, i] <- score
  }
  #sum the results for each racial group
  entropy <- rowSums(dat, na.rm = T)


  if(scaled == TRUE | thresholds == TRUE){
    #scale entropy between zero and log(number of races)
    entropy <- scale01(entropy, 0, log(length(races)))
    if(thresholds == TRUE){


      # sum two highest values from the race columns
      raceMatrix$highest2 <- apply(raceMatrix, 1, function(x){
        sum(sort(x, decreasing = T)[1:2], na.rm=T)
      })
      # Add entropy values to DF
      raceMatrix$entropy <- entropy

      raceMatrix$entropy_thresh = cut(entropy,
        # apply value cutoffs.
        breaks = c(-0.1,0.3707,0.7414,1), labels = FALSE)
      #apply population min/max filters
      raceMatrix$entropy_thresh = ifelse(do.call(pmax, c(raceMatrix[raceCols], na.rm = T)) >
          0.8, 1, raceMatrix$entropy_thresh)
      raceMatrix$entropy_thresh = ifelse((do.call(pmax, c(raceMatrix[raceCols], na.rm = T)) >
          0.45)
        # | highest2 > 0.8)
        & raceMatrix$entropy_thresh == 3, 2, raceMatrix$entropy_thresh)

      #save only the thresholds column
      entropy <- raceMatrix$entropy_thresh
    }
  }
  entropy
}
#' @rdname entropy
entropy_index <- function(entropy_smallGeo, entropy_bigGeo){
    (entropy_bigGeo - entropy_smallGeo) / entropy_bigGeo
}
#'
#' @rdname entropy
entropy_score <- function(entropy_index, totalPop){
  sum(entropy_index * (totalPop / sum(totalPop, na.rm=T)), na.rm=T)
}
