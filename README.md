
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rsegregation

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/arthurgailes/rsegregation.svg?branch=master)](https://travis-ci.org/arthurgailes/rsegregation)
[![Codecov test
coverage](https://codecov.io/gh/arthurgailes/rsegregation/branch/master/graph/badge.svg)](https://codecov.io/gh/arthurgailes/rsegregation?branch=master)
[![R build
status](https://github.com/arthurgailes/rsegregation/workflows/R-CMD-check/badge.svg)](https://github.com/arthurgailes/rsegregation/actions)
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

### Divergence and Entropy

#### Calculate the divergence score for the entire dataset

rsegregation can work with base r, or within several dplyr verbs:

``` r
library(rsegregation)
library(dplyr)
## included dataset of Bay Area Census tracts
# Using dplyr
bay_divergence <- bay_race %>% 
  summarize(bay_divergence = divergence(white,black,asian, hispanic, all_other), summed = T)

# Using base r
bay_divergence <- divergence(bay_race[c('white','black','asian', 'hispanic', 'all_other')], 
  summed = T)
# or
bay_divergence <- divergence(bay_race$white,bay_race$black,bay_race$asian, 
  bay_race$hispanic, bay_race$all_other, summed = T)
# all return the same result:
bay_divergence
```

#### Calculate divergence by group

Using the included Bay Area dataset of 2010 racial groups, divergence
can be calculated by county using `dplyr::group_by()`.

``` r
#library(dplyr)
group_by(bay_race, county) %>% 
  summarize(bay_divergence = divergence(white,black,asian, hispanic, all_other, summed = T))
```

<div class="kable-table">

| county                                 | bay\_divergence |
| :------------------------------------- | --------------: |
| Alameda County, California, 2010       |       0.2450583 |
| Contra Costa County, California, 2010  |       0.2129913 |
| Marin County, California, 2010         |       0.1304815 |
| Napa County, California, 2010          |       0.1459522 |
| San Francisco County, California, 2010 |       0.2056087 |
| San Mateo County, California, 2010     |       0.2387524 |
| Santa Clara County, California, 2010   |       0.2093378 |
| Solano County, California, 2010        |       0.1333189 |
| Sonoma County, California, 2010        |       0.0756877 |

</div>

#### By-observation divergence scores

Divergence and entropy are both calculated rowwise by default (summed =
FALSE).

``` r
bay_entropy <- bay_race
bay_entropy$entropy <- entropy(bay_race[c('white','black','asian',
  'hispanic','all_other')], summed = F)
head(bay_entropy)
```

<div class="kable-table">

| fips        | total\_pop | hispanic | white | black | asian | all\_other | county                           |   entropy |
| :---------- | ---------: | -------: | ----: | ----: | ----: | ---------: | :------------------------------- | --------: |
| 06001400100 |       2937 |      117 |  2078 |   140 |   456 |        146 | Alameda County, California, 2010 | 0.9566644 |
| 06001400200 |       1974 |      151 |  1546 |    31 |   146 |        100 | Alameda County, California, 2010 | 0.7969746 |
| 06001400300 |       4865 |      399 |  3256 |   512 |   419 |        279 | Alameda County, California, 2010 | 1.0859266 |
| 06001400400 |       3703 |      332 |  2424 |   448 |   270 |        229 | Alameda County, California, 2010 | 1.1121719 |
| 06001400500 |       3517 |      340 |  1778 |   933 |   208 |        258 | Alameda County, California, 2010 | 1.2816122 |
| 06001400600 |       1571 |      126 |   671 |   615 |    80 |         79 | Alameda County, California, 2010 | 1.2348325 |

</div>

### Miscellaneous

Dataframes should be formatted as long on geographic observations
(e.g. tracts), but wide on group observations (e.g. races), as in the
included dataset of the San Francisco Bay
Area.

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
