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


  if(file.exists(Filename)) { file.remove(Filename) }
  Shape <- c( Numrows, Numcols )
  H5File <- H5Fcreate( Filename )
  h5writeAttribute( 0.2, H5File, "OMX_VERSION" )
  h5writeAttribute( Shape, H5File, "SHAPE" )
  h5createGroup(Filename,"data")
  h5createGroup(Filename,"lookup")
  H5Fclose( H5File )
  TRUE
}

#Function to write OMX matrix data
#---------------------------------
#This function writes OMX matrix data. A full matrix can be written or just portions of an existing matrix. It allows overwriting existing matrix values, but only if the "Replace" argument is set to TRUE. If only portions of the matrix are to be written to, the full matrix must already exist.
#Arguments:
#OMXFileName = full path name of the OMX file to store the matrix in
#Matrix = matrix object to be stored
#MatrixSaveName = name under which the matrix will be saved in the OMX file
#RowIndex = vector of positional indexes that rows of matrix object are written to. NULL value means that all rows are written to.
#ColIndex = vector of positional indexes that columns of matrix object are written to. NULL value means that all columns are written to.
#NaValue = value that will be used to replace NA values in the matrix (NA is not a value that can be stored in OMX)
#Replace = TRUE or FALSE value to determine whether an existing matrix of the same name should be replaced by new matrix
#Description = String that describes the matrix
#Function returns TRUE if completed successfully
#Return: TRUE

writeMatrixOMX <- function( OMXFileName, Matrix, MatrixSaveName, RowIndex=NULL, ColIndex=NULL, NaValue=-1 ,
                            Replace=FALSE, Description="" ) {
  #Get names of matrices in the file vc
  Contents <- h5ls( OMXFileName )
  MatrixNames <- Contents$name[ Contents$group == "/data" ]
  MatrixExists <- MatrixSaveName %in% MatrixNames
  # Get the matrix dimensions specified in the file
  RootAttr <- getRootAttrOMX( OMXFileName )
  Shape <- RootAttr$SHAPE
  #Check whether there is matrix of that name already in the file
  if( MatrixExists & Replace == FALSE ){
    stop( paste("A matrix named '", MatrixSaveName, "' already exists. Value of 'Replace' argument must be TRUE in order to overwrite.", sep="") )
  }
  #Allow indexed writing (if RowIndex and ColIndex are not NULL) only if the matrix already exists
  if( !( is.null( RowIndex ) & is.null( ColIndex ) ) ){
    if( !MatrixExists ){
      stop( "Indexed writing to a matrix only allowed if a full matrix of that name already exists." )
    }
  }
  #If neither dimension will be written to indexes, write the full matrix and add the NA attribute
  if( is.null( RowIndex ) & is.null( ColIndex ) ){
    #Check conformance of matrix dimensions with OMX file
    if( !all( dim( Matrix ) == Shape ) ){
      stop( paste( "Matrix dimensions not consistent with", OMXFileName, ":", Shape[1], "Rows,", Shape[2], "Cols" ) )
    }
    #Transpose matrix and convert NA to designated storage value
    Matrix <- t( Matrix )
    Matrix[ is.na( Matrix ) ] <- NaValue
    #Write matrix to file
    ItemName <- paste( "data", MatrixSaveName, sep="/" )
    h5write( Matrix, OMXFileName, ItemName )
    #Add the NA storage value and matrix descriptions as attributes to the matrix
    H5File <- H5Fopen( OMXFileName )
    H5Group <- H5Gopen( H5File, "data" )
    H5Data <- H5Dopen( H5Group, MatrixSaveName )
    h5writeAttribute( NaValue, H5Data, "NA" )
    h5writeAttribute( Description, H5Data, "Description" )
    #Close everything up before exiting
    H5Dclose( H5Data )
    H5Gclose( H5Group )
    H5Fclose( H5File )
    #Otherwise write only to the indexed positions
  } else {
    if( is.null( RowIndex ) ) RowIndex <- 1:Shape[1]
    if( is.null( ColIndex ) ) ColIndex <- 1:Shape[2]
    #Check that indexes are within matrix dimension ranges
    if( any( RowIndex <= 0 ) | ( max( RowIndex ) > Shape[1] ) ){
      stop( "One or more values of 'RowIndex' are outside the index range of the matrix." )
    }
    if( any( ColIndex <= 0 ) | ( max( ColIndex ) > Shape[2] ) ){
      stop( "One or more values of 'ColIndex' are outside the index range of the matrix." )
    }
    #Check that there are no duplicated indices
    if( any( duplicated( RowIndex ) ) ){
      stop( "Duplicated index values in 'RowIndex'. Not permitted." )
    }
    if( any( duplicated( ColIndex ) ) ){
      stop( "Duplicated index values in 'ColIndex'. Not permitted." )
    }
    #Combine the row and column indexes into a list
    #Indices are reversed since matrix is stored in transposed form
    Indices <- list( RowIndex, ColIndex )
    #Transpose matrix and convert NA to designated storage value
    Matrix <- t( Matrix )
    Matrix[ is.na( Matrix ) ] <- NaValue
    # Write the matrix to the indexed positions
    ItemName <- paste( "data", MatrixSaveName, sep="/" )
    h5write( Matrix, OMXFileName, ItemName, index=Indices )
  }
  TRUE
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
readMatrixOMX <- function( OMXFileName, MatrixName, RowIndex=NULL, ColIndex=NULL ) {
  #Get the matrix dimensions specified in the file
  RootAttr <- getRootAttrOMX( OMXFileName )
  Shape <- RootAttr$SHAPE
  #Identify the item to be read
  ItemName <- paste( "data", MatrixName, sep="/" )
  #Check that RowIndex is proper
  if( !is.null( RowIndex ) ) {
    if( any( RowIndex <= 0 ) | ( max( RowIndex ) > Shape[1] ) ){
      stop( "One or more values of 'RowIndex' are outside the index range of the matrix." )
    }
    if( any( duplicated( RowIndex ) ) ){
      stop( "Duplicated index values in 'RowIndex'. Not permitted." )
    }
  }
  #Check that ColIndex is proper
  if( !is.null( ColIndex ) ) {
    if( any( ColIndex <= 0 ) | ( max( ColIndex ) > Shape[2] ) ){
      stop( "One or more values of 'ColIndex' are outside the index range of the matrix." )
    }
    if( any( duplicated( ColIndex ) ) ){
      stop( "Duplicated index values in 'ColIndex'. Not permitted." )
    }
  }
  #Combine the row and column indexes into a list
  #Indexes are reversed since matrix is stored in transposed form
  Indices <- list( ColIndex, RowIndex )
  #Read the indexed positions of the matrix
  Result <- t( h5read( OMXFileName, ItemName, index=list(ColIndex,RowIndex) ) )
  #Replace the NA values with NA
  NAValue = as.vector( attr( h5read( OMXFileName, ItemName, read.attribute=T ), "NA" ) )
  if(!is.null(NAValue)) {
    Result[ Result == NAValue ] <- NA
  }
  #Return the result
  Result
}
