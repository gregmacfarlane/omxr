OMX:  Open Matrix exchange format API for R
===

This is a reimplementation of the R API maintained by osPlanning and stored in a
[GitHub repository](https://github.com/osPlanning/omx/).

`omxr` is not yet on CRAN. To install it, first install the `devtools`
library. Then install `omxr` directly from GitHub.

    install_github("gregmacfarlane/omxr")

To install these functions in your workspace, 

    devtools
    
Note that these functions import on the rhdf5 v2.5.1+ package from bioconductor,
which is also not on CRAN. 



License 
-----------------
All code written in the OMX project, including all API implementations,
is under the Apache License,  version 2.0.

See LICENSE.TXT for the full Apache 2.0 license text.
