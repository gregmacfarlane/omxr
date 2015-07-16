#' Function to write a lookup vector to an OMX file
#'
#' This function writes a lookup vector to the file. It allows the user to
#' specify if the lookup vector applies only to rows or columns (in case the
#' matrix is not square and/or the rows and columns don't have the same
#' meanings.
#'
#' @param file Full path name of the OMX file to store the lookup.
#' @param lookup_v Vector containing the lookup information.
#' @param name Name for the lookup vector in \code{file}.
#' @param lookup_dim Matrix dimension that the lookup vector is associated with
#'   Values can be "row", "col", or NULL, meaning that the lookup is
#'   two-dimensional. Defaults to \code{NULL}.
#' @param replace Overwrite the existing lookup (if any)? Defaults to \code{FALSE}.
#' @param description
#'
#' @export
#' @import rhdf5
write_lookup <- function( file, lookup_v, name, lookup_dim=NULL, replace=FALSE, Description="" ) {
  #Check whether there is lookup of that name already in the file
  Contents = h5ls( file )
  LookupNames <- Contents$name[ Contents$group == "/lookup" ]
  if( ( name %in% LookupNames ) & ( replace == FALSE ) ){
    stop( paste("A lookup named '", name, "' already exists. 'replace' must be TRUE in order to overwrite.", sep="") )
  }
  #Error check lookup dimension arguments
  if( !is.null( lookup_dim ) ){
    if( !( lookup_dim %in% c( "row", "col" ) ) ) {
      stop( "lookup_dim argument must be 'row', 'col', or NULL." )
    }
  }
  Len <- length( lookup_v )
  RootAttr <- getRootAttrOMX( file )
  Shape <- RootAttr$SHAPE
  if( is.null( lookup_dim ) ) {
    if( Shape[1] != Shape[2] ) {
      stop( "Matrix is not square. You must specify the 'lookup_dim'" )
    }
    if( Len != Shape[1] ) {
      stop( paste( file, " has ", Shape[1], " rows and columns. lookup_v has ", Len, " positions.", sep="" ) )
    }
  }
  if( !is.null( lookup_dim ) ){
    if( lookup_dim == "row" ){
      if( Len != Shape[1] ){
        stop( paste( "Length of 'lookup_v' does not match row dimension of", file ) )
      }
    }
    if( lookup_dim == "col" ){
      if( Len != Shape[2] ){
        stop( paste( "Length of 'lookup_v' does not match column dimension of", file ) )
      }
    }
  }
  #Write lookup vector to file
  ItemName <- paste( "lookup", name, sep="/" )
  h5write( lookup_v, file, ItemName )
  #Write attributes
  H5File <- H5Fopen( file )
  H5Group <- H5Gopen( H5File, "lookup" )
  H5Data <- H5Dopen( H5Group, name )
  h5writeAttribute( Description, H5Data, "Description" )
  if( !is.null( lookup_dim ) ) {
    h5writeAttribute( lookup_dim, H5Data, "DIM" )
  }
  #Close everything up before exiting
  H5Dclose( H5Data )
  H5Gclose( H5Group )
  H5Fclose( H5File )
  TRUE
}


#Function to read an OMX lookup
#------------------------------
#This function reads a lookup and its attributes.
#Arguments:
#file = Path name of the OMX file where the lookup resides.
#LookupName = Name of the lookup in the OMX file
#Return: a list having 2 components
#Lookup = The lookup vector
#lookup_dim = The name of the matrix dimension the lookup corresponds to
readLookupOMX <- function( file, LookupName ) {
  #Identify the item to be read
  ItemName <- paste( "lookup", LookupName, sep="/" )
  #Read the lookup
  Lookup <- h5read( file, ItemName )
  #Read the name of the dimension the lookup corresponds
  H5File <- H5Fopen( file )
  H5Group <- H5Gopen( H5File, "lookup" )
  H5Data <- H5Dopen( H5Group, LookupName )
  if( H5Aexists( H5Data, "DIM" ) ) {
    H5Attr <- H5Aopen( H5Data, "DIM" )
    Dim <- H5Aread( H5Attr )
    H5Aclose( H5Attr )
  } else {
    Dim <- ""
  }
  H5Dclose( H5Data )
  H5Gclose( H5Group )
  H5Fclose( H5File )
  #Return the lookup and the corresponding dimension
  list( Lookup=Lookup, lookup_dim=Dim )
}

#Function to return portion of OMX matrix based using selection statements
#-------------------------------------------------------------------------
#This function reads a portion of an OMX matrix using selection statements to define the portion
#Multiple selection selection statements can be used for each dimension
#Each selection statement is a logical expression represented in a double-quoted string
#The left operand is the name of a lookup vector
#The operator can be any logical operator including %in%
#The right operand is the value or values to check against. This can be the name of a vector defined in the calling environment
#If the right operand contains literal string values, those values must be single-quoted
#Multiple selection conditions may be used as argument by including in a vector
#Multiple selection conditions are treated as intersections
#Arguments:
#file = Path name of the OMX file where the lookup resides.
#MatrixName = Name of the matrix in the OMX file
#row_selection = Row selection statement or vector of row selection statements (see above)
#col_selection = Column selection statement or vector of column selection statements (see above
#row_labels = Name of lookup to use for labeling rows
#ColLabels = Name of lookup to use for labeling columns
#Return: The selected matrix

readSelectedOMX <- function( file, MatrixName, row_selection=NULL, col_selection=NULL, row_labels=NULL, ColLabels=NULL ) {
  #Get the matrix dimensions specified in the file
  RootAttr <- getRootAttrOMX( file )
  Shape <- RootAttr$SHAPE
  #Define function to parse a selection statement and return corresponding data indices
  findIndex <- function( SelectionStmt ) {
    StmtParse <- unlist( strsplit( SelectionStmt, " " ) )
    IsBlank <- sapply( StmtParse, nchar ) == 0
    StmtParse <- StmtParse[ !IsBlank ]
    LookupName <- StmtParse[1]
    Lookup <- readLookupOMX( file, LookupName )
    assign( LookupName, Lookup[[1]]  )
    which( eval( parse( text=SelectionStmt ) ) )
  }
  #Make index for row selection
  if( !is.null( row_selection ) ) {
    row_index <- 1:Shape[1]
    for( Stmt in row_selection ) {
      Index <- findIndex( Stmt )
      row_index <- intersect( row_index, Index )
      rm( Index )
    }
  } else {
    row_index <- NULL
  }
  #Make index for column selection
  if( !is.null( col_selection ) ) {
    col_index <- 1:Shape[2]
    for( Stmt in col_selection ) {
      Index <- findIndex( Stmt )
      col_index <- intersect( col_index, Index )
      rm( Index )
    }
  } else {
    col_index <- NULL
  }
  #Extract the matrix meeting the selection criteria
  Result <- readMatrixOMX( file, MatrixName, row_index=row_index, col_index=col_index )
  #Label the rows and columns
  if( !is.null( row_labels ) ) {
    rownames( Result ) <- readLookupOMX( file, row_labels )[[1]][ row_index ]
  }
  if( !is.null( ColLabels ) ) {
    colnames( Result ) <- readLookupOMX( file, ColLabels )[[1]][ col_index ]
  }
  #Return the matrix
  Result
}
