
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
installed with:

``` r
# install.packages("devtools")
devtools::install_github("arthurgailes/rsegregation")
# dplyr is also suggested
# install.packages('dplyr')
```

## Usage

Return a single divergence score for bay County:

``` r
library(rsegregation)
library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
## basic example code
bay_divergence <- bay_race %>% 
  mutate(bay_divergence = divergence(white,black,asian, hispanic, all_other), summed = T)
```

### Base R Example

The same operation can be achieved in base R
with:

``` r
bay_divergence <- divergence(bay_race[c('white','black','asian', 'hispanic', 'all_other')], 
  summed = T)
```

or

``` r
bay_divergence <- divergence(bay_race$white,bay_race$black,bay_race$asian, 
  bay_race$hispanic, bay_race$all_other, summed = T)
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
