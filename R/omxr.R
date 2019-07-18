#' \code{omxr}: A package for using OMX files in R.
#' 
#' The Open Matrix (OMX) file type is an open specification for 
#' sharing data from transportation models. The specification is built on  
#' HDF5 \url{https://www.hdfgroup.org/};  
#' APIs to read and write are available for 'Cube', 'Emme', 'Python', 'Java',  
#' and 'R'.
#' 
#' @docType package
#' @name omxr
#' @importFrom dplyr %>%
#' @importFrom purrr %||%
#' 
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "value"))


#' Get path to example file
#' 
#' omxr comes bundled with an example omx file in its `inst/extdata`
#' directory. This function makes it easy to access.
#' 
#' @param path Name of file. If `NULL`, the example files will be listed.
#' @export
#' @examples
#' omxr_example()
#' omxr_example("skims.omx")
omxr_example <- function(path = NULL) {
  if (is.null(path)) {
    dir(system.file("extdata", package = "omxr"))
  } else {
    system.file("extdata", path, package = "omxr", mustWork = TRUE)
  }
}