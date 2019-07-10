#' Gather the destinations of a Origin-Destination matrix
#'
#' @param matrix An R matrix with row and column names or indices 
#' @param value_name A character string identifying the name of the value column in
#'   the output data_frame
#'
#' @return A \code{data_frame} with row and column ids and matrix values.
#'
#' @importFrom magrittr "%>%"
#' @importFrom rlang ":="
#' @importFrom dplyr rename
#' @importFrom purrr map_dfc
#'
#' @export
#'
gather_matrix <- function(matrix, value_name = NULL){
  
  # if no value name given, default to "value"
  if(is.null(value_name)) value_name <- "value"

  list(
    # set matrix row and column names if they exist
    origin = rownames(matrix)[row(matrix)] %||% row(matrix),
    destination = colnames(matrix)[col(matrix)] %||% col(matrix),
    value = matrix
  ) %>% 
    purrr::map_dfc(as.vector) %>%
    dplyr::rename(!!value_name := value)
  
}

#' Transform a matrix into long format
#' 
#' @inheritParams gather_matrix
#' 
#' 
#' @export
long_matrix <- function(matrix, value_name = NULL){
  .Deprecated("gather_matrix")
  gather_matrix(matrix, value_name)
}
