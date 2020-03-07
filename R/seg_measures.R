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
#' @source Created by Elizabeth Robert: <https://arxiv.org/abs/1508.01167>
#' @export
divergence <- function(..., totalPop = NULL, na.rm=TRUE, .sum=FALSE){

  races <- list(...)

  # convert list of vectors into DF, then convert to percentages
  raceMatrix <- dplyr::bind_cols(races)
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
#' Dissimilarity Score
#'
#' Stuff
#'
#' @inheritParams divergence
#'
#' @param group1,group2 Numeric vectors representing the popuplation of the
#' groups to be compared.
#'
#' @return A vector the length of group1 & group2
#'
#' @export
dissimilarity <- function(group1, group2, .sum=TRUE){
  dissim <- 0.5 * abs(group1/sum(group1, na.rm=T) - group2/sum(group2, na.rm=T))
  (dissim)
}
#' Exposure Index
#'
#' @inheritParams divergence
#' @inheritParams dissimilarity
#'
#' @export
exposure <- function(group1, group2, totalPop){
  expo <- ifelse(totalPop == 0, 0,
    ( (group1/sum(group1, na.rm=T)) * (group2/totalPop) )
  )
  (expo)
}
#' Isolation Index
#'
#' A single-group measure of the degree to which a group is isolated in
#' the full dataset
#'
#' @inheritParams divergence
#'
#' @param group A numeric vector of population
#'
#' @export
isolation <- function(group, totalPop){
  iso <- ifelse(totalPop == 0, 0,
    (group / sum(group, na.rm=T)) * (group / totalPop)
  )
  (iso)
}
#' Location Quotient
#'
#' @inheritParams divergence
#' @inheritParams isolation
#'
#' @export
location_quotient <- function(group, totalPop){
  (group / totalPop) / (sum(group, na.rm=T) / sum(totalPop, na.rm=T))
}
#' Theil's Index of Entropy
#'
#' \describe{
#'  \item{`entropy`}{Entropy score (Ei). `Ei = sum(Xim * log(1/Xim)` where Xim is the
#'  proportion of racial group within the geography. }
#'  \item{`entropy_score`}{Calculates the value of H (entropy index) for
#'  large-scale geography. }
#'  }
#'
#' @inheritParams divergence
#'
#' @name entropy
entropy <- function( ..., totalPop = NULL, scaled = FALSE, thresholds = FALSE){
  #each item in ... should be a vector of race populations
  races <- list(...)

  # convert list of vectors into DF, then convert to percentages
  raceMatrix <- dplyr::bind_cols(races)
  raceMatrix[is.na(raceMatrix)] <- 0
  #If total popuation is not provided, create from the sum of races
  if(is.null(totalPop)) totalPop <- apply(raceMatrix, 1, sum)
  raceMatrix <- raceMatrix / totalPop
  raceCols <- syms(colnames(raceMatrix))

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

      raceMatrix <- raceMatrix %>%
        mutate(entropy_thresh = cut(entropy,
          # apply value cutoffs.
          breaks = c(-0.1,0.3707,0.7414,1), labels = FALSE),
          #apply population min/max filters
          entropy_thresh = ifelse(pmax(!!!raceCols, na.rm = T) > 0.8, 1,
          entropy_thresh),
          entropy_thresh = ifelse((pmax(!!!raceCols, na.rm = T) > 0.45)
            # | highest2 > 0.8)
            & entropy_thresh == 3
            , 2, entropy_thresh)#,
          # high entropy means equal race values, so flip the scale for segregation
          # entropy_thresh = factor(entropy_thresh, 1:3, 3:1)
          )

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

# Helper function to create scale from 0-100
scale01 <- function(x, minimum = max(x, na.rm = T), maximum = max(x, na.rm = T)){
  (x-minimum)/(maximum-minimum)
}
