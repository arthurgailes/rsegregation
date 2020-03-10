# Helper function to create scale from 0-1
scale01 <- function(x, minimum = max(x, na.rm = T), maximum = max(x, na.rm = T)){
  (x-minimum)/(maximum-minimum)
}
