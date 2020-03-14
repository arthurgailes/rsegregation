
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsegregation

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/arthurgailes/rsegregation.svg?branch=master)](https://travis-ci.org/arthurgailes/rsegregation)
<!-- badges: end -->

This package is designed to fit into the tidyverse framework,
particularly dplyr.

## Installation

<!--
You can install the released version of rsegregation from [CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("rsegregation")
```
-->

The development version from [GitHub](https://github.com/) can be
installed withwith:

``` r
# install.packages("devtools")
devtools::install_github("arthurgailes/rsegregation")
# dplyr is also recommended
# install.packages('dplyr')
```

## Usage

Return a single divergence score for bay County:

``` r
library(rsegregation)
library(dplyr)
## basic example code
bay_divergence <- bay_race %>% 
  mutate(bay_divergence = divergence(white,black,asian))
  
```

## Base R Example

The same operation can be achieved in base R with:

``` r
summary(cars)
#>      speed           dist       
#>  Min.   : 4.0   Min.   :  2.00  
#>  1st Qu.:12.0   1st Qu.: 26.00  
#>  Median :15.0   Median : 36.00  
#>  Mean   :15.4   Mean   : 42.98  
#>  3rd Qu.:19.0   3rd Qu.: 56.00  
#>  Max.   :25.0   Max.   :120.00
```

### Misc

Dataframes should be formatted as long on geographic observations
(e.g. tracts), but wide on group observations (e.g. races), as in the
included dataset of bay County, CA

``` r
head(bay_wide)
```

### Split-Apply-Combine

rsegregation supports group\_by %\>% summarize operations from the dplyr
package.
