# Helper function to create scale from 0-1
scale01 <- function(x, minimum = min(x, na.rm = T), maximum = max(x, na.rm = T)){
  (x-minimum)/(maximum-minimum)
}

# sanity check whether numbers are percentages
sanity_check <- function(testvals, pct){
  minval <- min(testvals, na.rm=T)
  meanval <- mean(testvals, na.rm=T)
  if(minval < 0) warning("Negative numbers are not supported.")
  if(pct){
    if(meanval > 1) warning("It looks like you've entered
      total population numbers. If so, set `percentage` to FALSE.")
  } else {
    if(meanval > 0 & meanval < 1) warning("It looks like you've entered
      a percentage value. If so, set `percentage` to TRUE.")
  }
}
