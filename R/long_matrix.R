#' Transform a matrix into long format
#'
#' @param matrix An R matrix with row and column names or indices
#'
#' @return A \code{data_frame} with row and column ids and matrix values.
#'
#' @importFrom magrittr `%>%`
#'
#' @export
#'
long_matrix <- function(matrix){

  # set matrix row and column names if the exist
  names <- dimnames(matrix)
  if(is.null(names[1])){ rnames <- 1:nrow(matrix) } else { rnames <- names[[1]] }
  if(is.null(names[2])){ cnames <- 1:ncol(matrix) } else { cnames <- names[[2]] }


  # turn matrix into a data_frame with row names as an origin ID and
  # column names as variables
  df <- matrix %>% as.data.frame()
  names(df) <- cnames
  df <- dplyr::bind_cols(data_frame(origin = rnames), df)

  # longify
  df %>%
    tidyr::gather(destination, value, -origin, convert = TRUE)

}
