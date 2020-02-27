#' Segregation measures
#'
#' Six segregation measures
#'
#' @param totalPop The total population in the smallest geography being used
#'   (usually tracts).
#'
#'
#'
#' \describe{
#'  \item{`entropy`}{Entropy score (Ei). `Ei = sum(Xim * log(1/Xim)` where Xim is the
#'  proportion of racial group within the geography. }
#'  \item{`entropy_score`}{Calculates the value of H (entropy index) for
#'  large-scale geography. }
#'  \item{`zipped_shapefile`}{Extract shapefile directory and shapefile name}
#'  \item{`filepaths`}{Creates filenames for variables used within package for easy reading
#'    across years. Returns variables for reading when testing package functions; if used
#'    externally, they will be used in the global environment}
#'  \item{`access_file`} {Wrapper function that simplifies saving files
#'    into the proper directory.}
#'  \item{`censusVar`} {Wrapper for `paste0` that simplifies calling the variable with
#'    the correct year.}
#'
#' }
#'
#' @name segregation_measures
NULL
#' @rdname segregation_measures
dissim <- function(race1, race2){
  dissim <- 0.5 * abs(race1/sumna(race1) - race2/sumna(race2))
  (dissim)
}
#' @rdname segregation_measures
exposure <- function(race1, race2, totalPop){
  expo <- ifelse(totalPop == 0, 0,
    ( (race1/sumna(race1)) * (race2/totalPop) )
  )
  (expo)
}
#' @rdname segregation_measures
isolation <- function(race, totalPop){
  iso <- ifelse(totalPop == 0, 0,
    (race / sumna(race)) * (race / totalPop)
  )
  (iso)
}
#' @rdname segregation_measures
location_quotient <- function(race, totalPop){
  (race / totalPop) / (sumna(race) / sumna(totalPop))
}
#' @rdname segregation_measures
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
        sumna(sort(x, decreasing = T)[1:2])
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
#' @rdname segregation_measures
entropy_index <- function(entropy_smallGeo, entropy_bigGeo){
    (entropy_bigGeo - entropy_smallGeo) / entropy_bigGeo
}
#' @rdname segregation_measures
entropy_score <- function(entropy_index, totalPop){
  sumna(entropy_index * (totalPop / sumna(totalPop)))
}
#' @rdname segregation_measures
divergence <- function(..., totalPop = NULL){

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

#' @rdname segregation_measures
scale01 <- function(x, minimum = max(x, na.rm = T), maximum = max(x, na.rm = T)){
  (x-minimum)/(maximum-minimum)
  }
