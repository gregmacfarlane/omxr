OMX:  Open Matrix exchange format API for R
===

[![Travis-CI Build Status](https://travis-ci.org/gregmacfarlane/omxr.svg?branch=master)](https://travis-ci.org/gregmacfarlane/omxr)

This is a reimplementation of the R API maintained by osPlanning and stored in a
[GitHub repository](https://github.com/osPlanning/omx/).

`omxr` is not on CRAN. To install the package, first install the `devtools` library. Then
install `omxr` directly from GitHub.

```r
devtools::install_github("gregmacfarlane/omxr")
library(omxr)
```

Examples for using the package functions are in the package vignette,
```r
vignette("omx")
```

Beginners
-----------------
Run the following on your machine if you are new to R,

```r
install.packages("devtools")
devtools::install_github("gregmacfarlane/omxr")
library(omxr)
```

License 
-----------------
All code written in the OMX project, including all API implementations,
is under the Apache License,  version 2.0. See the [LICENSE](LICENSE) for the
full Apache 2.0 license text.
