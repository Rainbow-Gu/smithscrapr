
<!-- README.md is generated from README.Rmd. Please edit that file -->

# smithscrapr

<!-- badges: start -->

[![R-CMD-check](https://github.com/Rainbow-Gu/smithscrapr/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/Rainbow-Gu/smithscrapr/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

The goal of smithscrapr is to create a clear visualization for major
requirements on the Smith College website.

## Installation

You can install the development version of smithscrapr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("Rainbow-Gu/smithscrapr")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(smithscrapr)

sds_df <- req_df("sds")
sds_df
#> # A tibble: 6 × 3
#>   Requirement   Must                                                     Choose 
#>   <chr>         <chr>                                                    <chr>  
#> 1 Application   ""                                                       "A top…
#> 2 Capstone      "SDS 410"                                                ""     
#> 3 Communication ""                                                       "SDS 1…
#> 4 Core          "CSC 110, SDS 192, MTH 211, SDS 220 or SDS 201, SDS 291" ""     
#> 5 Programming   ""                                                       "CSC 2…
#> 6 Statistics    ""                                                       "SDS 2…
```
