#'  Create an OMX file
#'
#'  This function creates an OMX file, establishes the shape attribute (number
#'  of rows and columns) and version attribute, and creates the data and lookup
#'  groups.
#'
#'  @param file Full path name of the OMX file to create.
#'  @param numrows Number of rows that all matrices in the file will have.
#'  @param numcols Number of columns that all matrices in the file will have.
#'    Defaults to \code{numrows}.
#'  @param level Compression level. Default \code{1}.
#'
#'  @return An active connection to \code{file}
#'
#'  @export
#'  @import rhdf5
create_omx <- function(file, numrows, numcols, level = 1){

  # if file already exists, delete it
  if(file.exists(Filename)){
    file.remove(Filename)
  }

  # create hdf5 file with appropriate shape and attributes
  Shape <- c(numrows, numcols)
  H5File <- rhdf5::H5Fcreate(filename)
  rhdf5::h5writeAttribute(0.2, H5File, "OMX_VERSION")
  rhdf5::h5writeAttribute(Shape, H5File, "SHAPE")
  rhdf5::h5createGroup(Filename,"data")
  rhdf5::h5createGroup(Filename,"lookup")
  rhdf5::H5Fclose( H5File )

}


#' Function to write OMX matrix data
#'
#' This function writes OMX matrix data. A full matrix can be written or just
#' portions of an existing matrix. It allows overwriting existing matrix
#' values, but only if the "Replace" argument is set to TRUE. If only portions
#' of the matrix are to be written to, the full matrix must already exist.
#'
#' @param file Full path name of the OMX file to store the matrix in. If this is a new matrix file, see \link{create_omx}.
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
#' @export
#' @import rhdf
write_omx <- function(file, matrix, name,
                      row_index = NULL, col_index = NULL,
                      na_value = -1, replace = FALSE,
                      description = "") {

  #Get names of matrices in the file and check if exists
  Contents <- rhdf::h5ls(file)
  MatrixNames <- Contents$name[ Contents$group == "/data" ]

  if(name %in% MatrixNames & replace == FALSE ){
    stop(paste(
      "A matrix named '", MatrixSaveName,
      "' already exists. Value of 'Replace' argument must be TRUE in order to overwrite.",
      sep=""))
  }

  # Get the matrix dimensions specified in the file
  RootAttr <- getRootAttrOMX(file)
  Shape <- RootAttr$SHAPE


  #If neither dimension will be written to indexes, write the full matrix and
  # add the NA attribute
  if(is.null(row_index) & is.null(col_index) ){

    #Check conformance of matrix dimensions with OMX file
    if( !all( dim( Matrix ) == Shape ) ){
      stop(paste(
        "Matrix dimensions not consistent with", file, ":", Shape[1],
        "Rows,", Shape[2], "Cols"
      ))
    }

    #Transpose matrix and convert NA to designated storage value
    matrix <- t( matrix )
    matrix[ is.na( matrix ) ] <- na_value

    #Write matrix to file
    ItemName <- paste( "data", name, sep="/" )
    rhdf::h5write( Matrix, file, ItemName )

    #Add the NA storage value and matrix descriptions as attributes to the matrix
    H5File <- rhdf::H5Fopen( file )
    H5Group <- rhdf::H5Gopen( H5File, "data" )
    H5Data <- rhdf::H5Dopen( H5Group, name )
    h5writeAttribute(na_value, H5Data, "NA" )
    h5writeAttribute(description, H5Data, "Description" )

    #Close everything up before exiting
    rhdf::H5Dclose( H5Data )
    rhdf::H5Gclose( H5Group )
    hrdf::H5Fclose( H5File )

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
    rhdf::h5write(matrix, file, ItemName, index=Indices )

  }
}



#Function to read an OMX matrix
#------------------------------
#This function reads an entire matrix in an OMX file or portions of a matrix using indexing.
#Arguments:
#OMXFileName = Path name of the OMX file where the matrix resides.
#MatrixName = Name of the matrix in the OMX file
#RowIndex = Vector containing numerical indices of the rows to be read.
#ColIndex = Vector containing numerical indices of the columns to be read
#Return: a matrix
read_omx <- function(file, name, row_index = NULL, col_index = NULL){
  #Get the matrix dimensions specified in the file
  RootAttr <- getRootAttrOMX( file )
  Shape <- RootAttr$SHAPE
  #Identify the item to be read
  ItemName <- paste( "data", name, sep="/" )
  #Check that row_index is proper
  if( !is.null( row_index ) ) {
    if( any( row_index <= 0 ) | ( max( row_index ) > Shape[1] ) ){
      stop( "One or more values of 'row_index' are outside the index range of the matrix." )
    }
    if( any( duplicated( row_index ) ) ){
      stop( "Duplicated index values in 'row_index'. Not permitted." )
    }
  }
  #Check that col_index is proper
  if( !is.null( col_index ) ) {
    if( any( col_index <= 0 ) | ( max( col_index ) > Shape[2] ) ){
      stop( "One or more values of 'col_index' are outside the index range of the matrix." )
    }
    if( any( duplicated( col_index ) ) ){
      stop( "Duplicated index values in 'col_index'. Not permitted." )
    }
  }
  #Combine the row and column indexes into a list
  #Indexes are reversed since matrix is stored in transposed form
  Indices <- list( col_index, row_index )
  #Read the indexed positions of the matrix
  Result <- t( h5read( file, ItemName, index=list(col_index,row_index) ) )
  #Replace the NA values with NA
  NAValue = as.vector( attr( h5read( file, ItemName, read.attribute=T ), "NA" ) )
  if(!is.null(NAValue)) {
    Result[ Result == NAValue ] <- NA
  }
  #Return the result
  Result
}
