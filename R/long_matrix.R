#' Transform a matrix into long format
#'
#' @param matrix An R matrix with row and column names or indices
#' @param value A character string identifying the name of the value column in
#'   the output data_frame
#'
#' @return A \code{data_frame} with row and column ids and matrix values.
#'
#' @importFrom magrittr "%>%"
#' @importFrom dplyr bind_cols tbl_df
#' @importFrom tidyr gather
#' @importFrom readr type_convert
#'
#' @export
#'
long_matrix <- function(matrix, value = NULL){

  # set matrix row and column names if they exist
  names <- dimnames(matrix)
  if(is.null(names[1])){ rnames <- 1:nrow(matrix) } else { rnames <- names[[1]] }
  if(is.null(names[2])){ cnames <- 1:ncol(matrix) } else { cnames <- names[[2]] }

  # if no value name given, default to "value"
  if(is.null(value)) value <- "value"

  # turn matrix into a data_frame with row names as an origin ID and
  # column names as variables
  df <- matrix %>% as.data.frame()
  names(df) <- cnames
  dplyr::bind_cols(dplyr::data_frame(origin = rnames), df) %>%
    dplyr::tbl_df() %>%

    # longify
    tidyr::gather("destination", !!value, -origin, convert = TRUE) %>%
    readr::type_convert()

}
