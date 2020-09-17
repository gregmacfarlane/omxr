#' Create an OMX file
#'
#' This function creates an OMX file, establishes the shape attribute (number
#' of rows and columns) and version attribute, and creates the data and lookup
#' groups.
#
#' @param file Full path name of the OMX file to create.
#' @param numrows Number of rows that all matrices in the file will have.
#' @param numcols Number of columns that all matrices in the file will have.
#'    Defaults to \code{numrows}.
#' @param level Compression level. Default \code{1}.
#'
#' @return An active connection to \code{file}
#'
#' @importFrom rhdf5 h5writeAttribute h5createGroup H5Fclose H5Gclose H5Fclose
#'
#' @export
#' 
create_omx <- function(file, numrows, numcols, level = 1){

  # if file already exists, delete it
  if(file.exists(file)){
    file.remove(file)
  }

  # create hdf5 file with appropriate shape and attributes
  Shape <- c(numrows, numcols)
  H5File <- rhdf5::H5Fcreate(file)
  rhdf5::h5writeAttribute.character("0.2", H5File, "OMX_VERSION")
  rhdf5::h5writeAttribute.array(Shape, H5File, "SHAPE")
  rhdf5::h5createGroup(file,"data")
  rhdf5::h5createGroup(file,"lookup")
  rhdf5::H5Fclose( H5File )

}


#' Function to write OMX matrix data
#'
#' This function writes OMX matrix data. A full matrix can be written or just
#' portions of an existing matrix. It allows overwriting existing matrix
#' values, but only if the "Replace" argument is set to TRUE. If only portions
#' of the matrix are to be written to, the full matrix must already exist.
#'
#' @param file Full path name of the OMX file to store the matrix in. If this is
#'   a new matrix file, see \link{create_omx}.
#' @param matrix Matrix object to be stored.
#' @param name Name of the matrix in the OMX object.
#' @param row_index An integer vector indicating the rows represented by
#'   \code{matrix}. Defaults to \code{NULL}, meaning that all rows are written
#'   (and \code{nrow(matrix)} is the number of rows in the matrix).
#' @param col_index The precise corrolary to \code{row_index}, for columns.
#' @param na_value The value representing \code{NA} values in the matrix.
#'   Defaults to \code{-1}
#' @param replace If the named object already exists in \code{file}, should it
#'   be replaced? Defaults to \code{FALSE}.
#' @param description (Optional) description of matrix contents.
#' 
#' @return A return code indicated the success (0) or failure of the write process.
#'
#' @importFrom rhdf5 h5ls h5writeAttribute h5createDataset h5writeDataset
#'   H5Dclose h5write H5Fopen H5Gopen H5Dopen 
#' @export
#' 
#' @examples 
#' omxfile <- tempfile(fileext = ".omx")
#' create_omx(omxfile, 10, 10)
#' trips <- matrix(rnorm(n = 10^2, 200, 50),   
#'                 nrow = 10, ncol = 10)
#' write_omx(matrix = trips, omxfile, "trips",  description = "Total Trips")
#'
write_omx <- function(matrix, file,  name,
                      row_index = NULL, col_index = NULL,
                      na_value = -1, replace = FALSE,
                      description = "") {

  #Get names of matrices in the file and check if exists
  H5File <- rhdf5::H5Fopen(file)
  Contents <- rhdf5::h5ls(H5File)
  MatrixNames <- Contents$name[ Contents$group == "/data" ]
  MatrixExists <- name %in% MatrixNames

  if(name %in% MatrixNames & replace == FALSE ){
    stop(paste(
      "Matrix", name, "already exists. To overwrite, set 'replace = TRUE"
    ))
  }

  # Get the matrix dimensions specified in the file
  RootAttr <- get_omx_attr(file)
  Shape <- RootAttr$SHAPE

  #Allow indexed writing only if the matrix already exists
  if( !( is.null(row_index) & is.null(col_index) ) ){
    if( !MatrixExists ){
      stop(
        "Indexed writing to a matrix only allowed if a full matrix of that name already exists."
      )
    }
  }

  #If neither dimension will be written to indexes, write the full matrix and
  # add the NA attribute
  if(is.null(row_index) & is.null(col_index) ){

    #Check conformance of matrix dimensions with OMX file
    if( !all( dim( matrix ) == Shape ) ){
      stop(paste(
        "Matrix dimensions not consistent with", file, ":", Shape[1],
        "Rows,", Shape[2], "Cols"
      ))
    }

    #Transpose matrix and convert NA to designated storage value
    matrix <- t( matrix )
    matrix[ is.na( matrix ) ] <- na_value

    #Write matrix to file, set chunking and compression
    ItemName <- paste( "data", name, sep="/" )
    rhdf5::h5createDataset(H5File, matrix, ItemName,
      dim(matrix), chunk=c(nrow(matrix), 1),
      level=7
    )
    rhdf5::h5writeDataset.matrix(matrix, H5File, ItemName)

    #Add the NA storage value and matrix descriptions as attributes to the matrix
    H5Group <- rhdf5::H5Gopen( H5File, "data" )
    H5Data <- rhdf5::H5Dopen( H5Group, name )
    rhdf5::h5writeAttribute.double(na_value, H5Data, "NA" )
    rhdf5::h5writeAttribute.character(description, H5Data, "Description" )

    #Close everything up before exiting
    rhdf5::H5Dclose( H5Data )
    rhdf5::H5Gclose( H5Group )
    rhdf5::H5Fclose( H5File )

  } else {
    #Otherwise write only to the indexed positions

    # set dimensions
    if( is.null( row_index ) ) row_index <- 1:Shape[1]
    if( is.null( col_index ) ) col_index <- 1:Shape[2]

    #Check that indexes are within matrix dimension ranges
    if( any( row_index <= 0 ) | ( max( row_index ) > Shape[1] ) ){
      stop("Elements of 'row_index' are outside the index range of the matrix.")
    }
    if( any( col_index <= 0 ) | ( max( col_index ) > Shape[2] ) ){
      stop("Elements of 'col_index' are outside the index range of the matrix.")
    }

    #Check that there are no duplicated indices
    if( any( duplicated( row_index ) ) ){
      stop( "Duplicated index values in 'row_index' not permitted." )
    }
    if( any( duplicated( col_index ) ) ){
      stop( "Duplicated index values in 'col_index' not permitted." )
    }

    #Combine the row and column indexes into a list
    #Indices are reversed since matrix is stored in transposed form
    Indices <- list( row_index, col_index )

    #Transpose matrix and convert NA to designated storage value
    matrix <- t( matrix )
    matrix[ is.na( matrix ) ] <- na_value

    # Write the matrix to the indexed positions
    ItemName <- paste( "data", name, sep="/" )
    rhdf5::h5writeDataset(matrix, H5File, ItemName, index=Indices )

  }
}

#' Read an OMX matrix
#'
#' This function reads an entire matrix in an OMX file or portions of a matrix
#' using indexing.
#'
#' @param file Path name of the OMX file where the matrix resides.
#' @param name Name of the matrix in the OMX file
#' @param row_index An integer vector indicating the rows represented by
#'   \code{matrix}. Defaults to \code{NULL}, meaning that all rows are read
#'   (and \code{nrow(matrix)} is the number of rows in the matrix).
#' @param col_index The precise corrolary to \code{row_index}, for columns.
#'
#' @return an R matrix object
#'
#' @importFrom rhdf5 h5read
#'
#' @export
#' @examples
#' omxfile <- omxr_example("skims.omx")
#' mat <- read_omx(omxfile, "DIST")
#' str(mat)
#' read_omx(omxfile, "DIST", c(1,4,5), c(1,4,5))
#'
read_omx <- function(file, name, row_index = NULL, col_index = NULL){

  #Get the matrix dimensions specified in the file
  RootAttr <- omxr::get_omx_attr( file )
  Shape <- RootAttr$SHAPE

  #Identify the item to be read
  ItemName <- paste( "data", name, sep="/" )

  #Check that indices are properly formatted
  Indices <- list(row_index, col_index)
  dim <- 1   # initialize dimension
  for(index in Indices){

    if(!is.null(index)) {
      if(any(index <= 0) | (max(index) > Shape[dim])){
        stop(paste("One or more values in dimension", dim, "are invalid."))
      }

      if(any(duplicated(index))){
        stop("Duplicated index values in dimension", dim, ".")
      }
    }

    # change dimension from 1 to 2
    dim <- dim + 1
  }


  #Read the indexed positions of the matrix
  Result <- t(rhdf5::h5read(file, ItemName, index = rev(Indices)))

  #Replace the NA values with NA
  NAValue = as.vector(
    attr(rhdf5::h5read(file, ItemName, read.attribute=T),  "NA")
  )

  if(!is.null(NAValue)) {
    Result[Result == NAValue] <- NA
  }

  Result
}




#' Read all matrix cores from an OMX file
#' 
#' @param file Path to OMX file
#' @param names A character vector of matrices to read. If blank, will read all.
#' @param long If TRUE (default) will return the matrices as a long tidy 
#'   tibble with the cores as columns. If FALSE will return a list of named
#'   matrices.
#'   
#' @importFrom tidyr spread
#' @importFrom dplyr bind_rows
#' @importFrom stats setNames
#' 
#' @return Depending on the value of `long`: if TRUE, a tibble with one core
#'   per column and one row per interchange; if FALSE, a list of named matrices.
#' @export
#' @examples 
#' omxfile <- omxr_example("skims.omx")
#' read_all_omx(omxfile)[, c("origin", "destination", "DIST", "DISTBIKE", "DISTWALK")]
#' read_all_omx(omxfile, long = FALSE) [c("DIST", "DISTBIKE", "DISTWALK")]
#' 
#'   
#' 
read_all_omx <- function(file, names = NULL, long = TRUE) {
  core_names <- list_omx(file)[["Matrices"]][["name"]]
  
  if(!is.null(names)){
    core_names <- core_names[which(core_names %in% names)]
  }
  
  matrices <- lapply(core_names, function(name) read_omx(file, name)) %>%
    stats::setNames(core_names)
  
  if (long) {
    matrices <- lapply(matrices, gather_matrix) %>%
      dplyr::bind_rows(.id = "matrix") %>%
      tidyr::spread(matrix, value)
  }
  
  matrices
}


#' Write all matrix cores to an OMX file
#' 
#' @param object The object to be written to OMX. May be either a tibble or 
#'   a list of named matrices.
#' @param long Identify whether the object to be written is a long-format 
#'   `tibble` (the default, `TRUE`) or a list of named matrices.
#' @param file The location of the OMX file.
#' 
#' @return A list with a return code for each matrix element written to OMX. 
#' 
#' @export
#' 
#' @examples 
#' omxfile <- omxr_example("skims.omx")
#' # long-format (tibble)
#' skims <- read_all_omx(omxfile)[, c("origin", "destination", "DIST", "DISTBIKE", "DISTWALK")]
#' write_all_omx(skims, file = tempfile(fileext = ".omx"))
#' 
#' # list of matrices
#' skims <- read_all_omx(omxfile, long = FALSE)[c("DIST", "DISTBIKE", "DISTWALK")]
#' write_all_omx(skims, file = tempfile(fileext = ".omx"), long = FALSE)
#' 
#' @importFrom dplyr select
#' @importFrom tidyr spread
#' 
#' 
write_all_omx <- function(object, file, long = TRUE) {
  
  # get matrix parameters to create a new omx file
  if(long) {   # Tibble input
    numrows <- length(unique(object[["origin"]]))
    numcols <- length(unique(object[["destination"]]))
    core_names <- colnames(object)[!(colnames(object) %in% c("origin", "destination"))]
    
  } else {     # Matrix list input
    numrows <- nrow(object[[1]])
    numcols <- ncol(object[[1]])
    core_names <- names(object)
  }
  
  # create the omx file
  create_omx( file, numrows = numrows, numcols = numcols)
  
  # write matrix information to OMX.
  if (long) { # Tibble input
    lapply(core_names, function(table_name) {
      object %>%
        dplyr::select(origin, destination, !!table_name ) %>%
        tidyr::spread(destination, !!table_name) %>%
        dplyr::select(-origin) %>% as.matrix() %>%
        write_omx(file = file, matrix = ., table_name)
    })
  } else {     # Matrix list input
    lapply(core_names, function(table_name){ 
      write_omx(matrix = object[[table_name]], file = file, name = table_name)
    })
  }
  
}
