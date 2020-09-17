
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
omx_file <- omxr_example("skims.omx")

# list contents
list_omx(omx_file)
#> $OMXVersion
#> [1] "0.2"
#> 
#> $Rows
#> [1] 25
#> 
#> $Columns
#> [1] 25
#> 
#> $Matrices
#>                      name dclass     dim   type
#> 1                    DIST  FLOAT 25 x 25 matrix
#> 2                DISTBIKE  FLOAT 25 x 25 matrix
#> 3                DISTWALK  FLOAT 25 x 25 matrix
#> 4   DRV_LOC_WLK_DDIST__AM  FLOAT 25 x 25 matrix
#> 5    DRV_LOC_WLK_DTIM__AM  FLOAT 25 x 25 matrix
#> 6     DRV_LOC_WLK_FAR__AM  FLOAT 25 x 25 matrix
#> 7   DRV_LOC_WLK_IWAIT__AM  FLOAT 25 x 25 matrix
#> 8  DRV_LOC_WLK_TOTIVT__AM  FLOAT 25 x 25 matrix
#> 9    DRV_LOC_WLK_WAIT__AM  FLOAT 25 x 25 matrix
#> 10   DRV_LOC_WLK_WAUX__AM  FLOAT 25 x 25 matrix
#> 11  DRV_LOC_WLK_XWAIT__AM  FLOAT 25 x 25 matrix
#> 12     HOV2TOLL_BTOLL__AM  FLOAT 25 x 25 matrix
#> 13      HOV2TOLL_DIST__AM  FLOAT 25 x 25 matrix
#> 14      HOV2TOLL_TIME__AM  FLOAT 25 x 25 matrix
#> 15     HOV2TOLL_VTOLL__AM  FLOAT 25 x 25 matrix
#> 16         HOV2_BTOLL__AM  FLOAT 25 x 25 matrix
#> 17          HOV2_DIST__AM  FLOAT 25 x 25 matrix
#> 18          HOV2_TIME__AM  FLOAT 25 x 25 matrix
#> 19         HOV3_BTOLL__AM  FLOAT 25 x 25 matrix
#> 20          HOV3_DIST__AM  FLOAT 25 x 25 matrix
#> 21          HOV3_TIME__AM  FLOAT 25 x 25 matrix
#> 22      SOVTOLL_BTOLL__AM  FLOAT 25 x 25 matrix
#> 23       SOVTOLL_DIST__AM  FLOAT 25 x 25 matrix
#> 24       SOVTOLL_TIME__AM  FLOAT 25 x 25 matrix
#> 25      SOVTOLL_VTOLL__AM  FLOAT 25 x 25 matrix
#> 26          SOV_BTOLL__AM  FLOAT 25 x 25 matrix
#> 27           SOV_DIST__AM  FLOAT 25 x 25 matrix
#> 28           SOV_TIME__AM  FLOAT 25 x 25 matrix
#> 29 WLK_LOC_DRV_BOARDS__AM  FLOAT 25 x 25 matrix
#> 30  WLK_LOC_DRV_DDIST__AM  FLOAT 25 x 25 matrix
#> 31   WLK_LOC_DRV_DTIM__AM  FLOAT 25 x 25 matrix
#> 32    WLK_LOC_DRV_FAR__AM  FLOAT 25 x 25 matrix
#> 33  WLK_LOC_DRV_IWAIT__AM  FLOAT 25 x 25 matrix
#> 34 WLK_LOC_DRV_TOTIVT__AM  FLOAT 25 x 25 matrix
#> 35   WLK_LOC_DRV_WAIT__AM  FLOAT 25 x 25 matrix
#> 36   WLK_LOC_DRV_WAUX__AM  FLOAT 25 x 25 matrix
#> 37  WLK_LOC_DRV_XWAIT__AM  FLOAT 25 x 25 matrix
#> 38 WLK_LOC_WLK_BOARDS__AM  FLOAT 25 x 25 matrix
#> 39    WLK_LOC_WLK_FAR__AM  FLOAT 25 x 25 matrix
#> 40  WLK_LOC_WLK_IWAIT__AM  FLOAT 25 x 25 matrix
#> 41 WLK_LOC_WLK_TOTIVT__AM  FLOAT 25 x 25 matrix
#> 42   WLK_LOC_WLK_WAIT__AM  FLOAT 25 x 25 matrix
#> 43   WLK_LOC_WLK_WAUX__AM  FLOAT 25 x 25 matrix
#> 44  WLK_LOC_WLK_XWAIT__AM  FLOAT 25 x 25 matrix
#> 45    WLK_TRN_WLK_IVT__AM  FLOAT 25 x 25 matrix
#> 46    WLK_TRN_WLK_IVT__MD  FLOAT 25 x 25 matrix
#> 47    WLK_TRN_WLK_IVT__PM  FLOAT 25 x 25 matrix
#> 48  WLK_TRN_WLK_IWAIT__AM  FLOAT 25 x 25 matrix
#> 49   WLK_TRN_WLK_WACC__AM  FLOAT 25 x 25 matrix
#> 50   WLK_TRN_WLK_WAUX__AM  FLOAT 25 x 25 matrix
#> 51   WLK_TRN_WLK_WEGR__AM  FLOAT 25 x 25 matrix
#> 52  WLK_TRN_WLK_XWAIT__AM  FLOAT 25 x 25 matrix
#> 
#> $Lookups
#>         name  dclass dim lookupdim                 description
#> 54 Districts INTEGER  25                             Districts
#> 55        EI  STRING  25           External and Internal Zones
```

You can choose to read the matrix as either an R matrix or as a
tidyverse-friendly tibble, depending on your needs.

``` r
m <- read_omx(omx_file, "DIST") # return an array
m[1:5, 1:5]
#>      [,1] [,2] [,3] [,4] [,5]
#> [1,] 0.12 0.24 0.44 0.41 0.68
#> [2,] 0.37 0.14 0.28 0.29 0.59
#> [3,] 0.57 0.28 0.14 0.46 0.45
#> [4,] 0.34 0.24 0.33 0.12 0.35
#> [5,] 0.70 0.64 0.52 0.40 0.20

read_all_omx(omx_file, c("DIST", "DISTBIKE", "DISTWALK"))
#> # A tibble: 625 x 5
#>    origin destination  DIST DISTBIKE DISTWALK
#>     <int>       <int> <dbl>    <dbl>    <dbl>
#>  1      1           1  0.12     0.12     0.12
#>  2      1           2  0.24     0.24     0.24
#>  3      1           3  0.44     0.44     0.44
#>  4      1           4  0.41     0.41     0.41
#>  5      1           5  0.68     0.68     0.68
#>  6      1           6  0.97     0.97     0.97
#>  7      1           7  0.98     0.98     0.98
#>  8      1           8  1.15     1.15     1.15
#>  9      1           9  1.56     1.56     1.56
#> 10      1          10  1.59     1.59     1.59
#> # … with 615 more rows
```

More detailed examples, including creating and writing to OMX files, are
available in the package vignette.

``` r
vignette(omxr)
```
