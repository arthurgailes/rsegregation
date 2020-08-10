
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsegregation

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/arthurgailes/rsegregation.svg?branch=master)](https://travis-ci.org/arthurgailes/rsegregation)
<!-- badges: end -->

rsegregation is designed to fit into the tidyverse framework,
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
```

## Usage

rsegregation depends upon dplyr (\>1.0.0), and can be used with it. To
return a single divergence score for Bay Area County:

#### Using dplyr

``` r
library(rsegregation)
library(dplyr)
## included dataset of Bay Area Census tracts
bay_divergence <- bay_race %>% 
  summarize(bay_divergence = divergence(white,black,asian, hispanic, all_other), summed = T)
```

#### Using Base r

``` r
bay_divergence <- divergence(bay_race[c('white','black','asian', 'hispanic', 'all_other')], 
  summed = T)
```

or

``` r
bay_divergence <- divergence(bay_race$white,bay_race$black,bay_race$asian, 
  bay_race$hispanic, bay_race$all_other, summed = T)
```

    #> [1] 0.2575266

### Miscellaneous

Dataframes should be formatted as long on geographic observations
(e.g. tracts), but wide on group observations (e.g. races), as in the
included dataset of bay County,
CA

``` r
head(bay_race)
```

<div class="kable-table">

| fips        | total\_pop | hispanic | white | black | asian | all\_other | county                           |
| :---------- | ---------: | -------: | ----: | ----: | ----: | ---------: | :------------------------------- |
| 06001400100 |       2937 |      117 |  2078 |   140 |   456 |        146 | Alameda County, California, 2010 |
| 06001400200 |       1974 |      151 |  1546 |    31 |   146 |        100 | Alameda County, California, 2010 |
| 06001400300 |       4865 |      399 |  3256 |   512 |   419 |        279 | Alameda County, California, 2010 |
| 06001400400 |       3703 |      332 |  2424 |   448 |   270 |        229 | Alameda County, California, 2010 |
| 06001400500 |       3517 |      340 |  1778 |   933 |   208 |        258 | Alameda County, California, 2010 |
| 06001400600 |       1571 |      126 |   671 |   615 |    80 |         79 | Alameda County, California, 2010 |

</div>

## Future development:

  - decomposition of entropy index
  - more measures of segregation

## License

This package is free and open source software, licensed under GPL-3.
