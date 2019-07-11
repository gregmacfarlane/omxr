
<!-- README.md is generated from README.Rmd. Please edit that file -->

# omxr

[![CRAN
status](https://www.r-pkg.org/badges/version/omxr)](https://cran.r-project.org/package=omxr)
[![Build
Status](https://travis-ci.org/gregmacfarlane/omxr.svg?branch=master)](https://travis-ci.org/gregmacfarlane/omxr)

The goal of omxr is to provide a convenient API for R users working with
Open Matrix (OMX) format files. For more details on the file standard,
see the [osPlanning wiki](https://github.com/osPlanning/omx/wiki).

## Installation

We hope to have omxr on [CRAN](https://CRAN.R-project.org) in the near
future. Until then, you can install omxr with the
[devtools](https://cran.r-project.org/package=devtools) package

``` r
devtools::install_github("gregmacfarlane/omxr")
```

## Example

With omxr you can examine the contents of existing OMX files within R.

``` r
# path to the omx file
omx_file <- system.file("extdata", "test.omx", package = "omxr")

# list contents
list_omx(omx_file)
#> $OMXVersion
#> [1] 0.2
#> 
#> $Rows
#> [1] 524
#> 
#> $Columns
#> [1] 524
#> 
#> $Matrices
#>   name dclass       dim   type navalue description
#> 1 test  FLOAT 524 x 524 matrix      -1            
#> 
#> $Lookups
#>   name dclass dim lookupdim description
#> 3   NO STRING 524
```

You can choose to read the matrix as either an R matrix or as a
tidyverse-friendly tibble, depending on your needs.

``` r
m <- read_omx(omx_file, "test") # return an array
m[1:5, 1:5]
#>            [,1]       [,2]      [,3]      [,4]      [,5]
#> [1,] -0.2537709 -1.8928944 -5.675423 -2.592095 -6.441926
#> [2,] -1.8928940 -0.8478473 -4.370062 -2.701055 -6.267865
#> [3,] -5.6754227 -4.3700619 -4.078290 -4.440516 -5.653083
#> [4,] -2.5920970 -2.7010546 -4.440515 -1.371779 -2.659490
#> [5,] -6.4419241 -6.2678657 -5.653086 -2.659489 -1.499711

read_all_omx(omx_file)
#> # A tibble: 274,576 x 3
#>    origin destination    test
#>     <int>       <int>   <dbl>
#>  1      1           1  -0.254
#>  2      1           2  -1.89 
#>  3      1           3  -5.68 
#>  4      1           4  -2.59 
#>  5      1           5  -6.44 
#>  6      1           6  -7.21 
#>  7      1           7  -6.10 
#>  8      1           8  -8.31 
#>  9      1           9 -12.0  
#> 10      1          10  -8.24 
#> # … with 274,566 more rows
```

More detailed examples, including creating and writing to OMX files, are
available in the package vignette.

``` r
vignette(omxr)
```
