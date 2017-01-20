OMX:  Open Matrix exchange format API for R
===

This is a reimplementation of the R API maintained by osPlanning and stored in a
[GitHub repository](https://github.com/osPlanning/omx/).

`omxr` is not on CRAN. To install the package, first install the `devtools` library. Then
install `omxr` directly from GitHub.

```r
devtools::install_github("gregmacfarlane/omxr")
library(omxr)
```
    
Note that `omxr` functions import the `rhdf5` v2.5.1+ package from
[Bioconductor](http://bioconductor.org/packages/release/bioc/html/rhdf5.html),
which is also not on CRAN. To install this library, 

```r
source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")
```

Examples for using the package functions are in the package vignette,

```r
vignette("omx")
```

License 
-----------------
All code written in the OMX project, including all API implementations,
is under the Apache License,  version 2.0. See the [LICENSE](LICENSE) for the
full Apache 2.0 license text.
