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