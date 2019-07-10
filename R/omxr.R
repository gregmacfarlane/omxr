#' \code{omxr} package
#' 
#' API for OMX matrices in R.
#' 
#' @docType package
#' @name omxr
#' @importFrom dplyr %>%
#' @importFrom purrr %||%
#' 
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c(".", "value"))