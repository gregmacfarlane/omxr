OMX:  Open Matrix exchange format API for R
===

This is a reimplementation of the R API maintained by osPlanning and stored in a
[GitHub repository](https://github.com/osPlanning/omx/).

`omxr` is not yet on CRAN. To install it, first install the `devtools`
library. Then install `omxr` directly from GitHub.

    devtools::install_github("gregmacfarlane/omxr")

To install these functions in your workspace, 

    library(omxr)

Note that these functions import on the `rhdf5` v2.5.1+ package from
[Bioconductor](http://bioconductor.org/packages/release/bioc/html/rhdf5.html),
which is also not on CRAN. If you do not already have this library installed, run 
the following on your machine once,

    source("http://bioconductor.org/biocLite.R")
    biocLite("rhdf5")

Beginners
-----------------
Run the following on your machine if you are new to R,

    source("http://bioconductor.org/biocLite.R")
    biocLite("rhdf5")
    install_packages("devtools")
    devtools::install_github("gregmacfarlane/omxr")
    library(omxr)

For help getting started with the package, run

    vignette(omxr)


License 
-----------------
All code written in the OMX project, including all API implementations,
is under the Apache License,  version 2.0.

See LICENSE.TXT for the full Apache 2.0 license text.
