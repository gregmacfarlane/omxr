#Function to write a lookup vector to an OMX file
#------------------------------------------------
#This function writes a lookup vector to the file. It allows the user to specify if the lookup vector applies only to rows or columns (in case the matrix is not square and/or the rows and columns don't have the same meanings.
#Arguments:
#OMXFileName = full path name of the OMX file to store the lookup vector in
#LookupVector = lookup vector object to be stored
#LookupSaveName = name under which the lookup vector will be saved in the OMX file
#LookupDim = matrix dimension that the lookup vector is associated with
#Values can be "row", "col", or NULL. A lookup dimension attribute is optional.
#Description = string that describes the matrix
#Return: TRUE
writeLookupOMX <- function( OMXFileName, LookupVector, LookupSaveName, LookupDim=NULL, Replace=FALSE, Description="" ) {
  #Check whether there is lookup of that name already in the file
  Contents = h5ls( OMXFileName )
  LookupNames <- Contents$name[ Contents$group == "/lookup" ]
  if( ( LookupSaveName %in% LookupNames ) & ( Replace == FALSE ) ){
    stop( paste("A lookup named '", LookupSaveName, "' already exists. 'Replace' must be TRUE in order to overwrite.", sep="") )
  }
  #Error check lookup dimension arguments
  if( !is.null( LookupDim ) ){
    if( !( LookupDim %in% c( "row", "col" ) ) ) {
      stop( "LookupDim argument must be 'row', 'col', or NULL." )
    }
  }
  Len <- length( LookupVector )
  RootAttr <- getRootAttrOMX( OMXFileName )
  Shape <- RootAttr$SHAPE
  if( is.null( LookupDim ) ) {
    if( Shape[1] != Shape[2] ) {
      stop( "Matrix is not square. You must specify the 'LookupDim'" )
    }
    if( Len != Shape[1] ) {
      stop( paste( OMXFileName, " has ", Shape[1], " rows and columns. LookupVector has ", Len, " positions.", sep="" ) )
    }
  }
  if( !is.null( LookupDim ) ){
    if( LookupDim == "row" ){
      if( Len != Shape[1] ){
        stop( paste( "Length of 'LookupVector' does not match row dimension of", OMXFileName ) )
      }
    }
    if( LookupDim == "col" ){
      if( Len != Shape[2] ){
        stop( paste( "Length of 'LookupVector' does not match column dimension of", OMXFileName ) )
      }
    }
  }
  #Write lookup vector to file
  ItemName <- paste( "lookup", LookupSaveName, sep="/" )
  h5write( LookupVector, OMXFileName, ItemName )
  #Write attributes
  H5File <- H5Fopen( OMXFileName )
  H5Group <- H5Gopen( H5File, "lookup" )
  H5Data <- H5Dopen( H5Group, LookupSaveName )
  h5writeAttribute( Description, H5Data, "Description" )
  if( !is.null( LookupDim ) ) {
    h5writeAttribute( LookupDim, H5Data, "DIM" )
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
#OMXFileName = Path name of the OMX file where the lookup resides.
#LookupName = Name of the lookup in the OMX file
#Return: a list having 2 components
#Lookup = The lookup vector
#LookupDim = The name of the matrix dimension the lookup corresponds to
readLookupOMX <- function( OMXFileName, LookupName ) {
  #Identify the item to be read
  ItemName <- paste( "lookup", LookupName, sep="/" )
  #Read the lookup
  Lookup <- h5read( OMXFileName, ItemName )
  #Read the name of the dimension the lookup corresponds
  H5File <- H5Fopen( OMXFileName )
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
  list( Lookup=Lookup, LookupDim=Dim )
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
#OMXFileName = Path name of the OMX file where the lookup resides.
#MatrixName = Name of the matrix in the OMX file
#RowSelection = Row selection statement or vector of row selection statements (see above)
#ColSelection = Column selection statement or vector of column selection statements (see above
#RowLabels = Name of lookup to use for labeling rows
#ColLabels = Name of lookup to use for labeling columns
#Return: The selected matrix

readSelectedOMX <- function( OMXFileName, MatrixName, RowSelection=NULL, ColSelection=NULL, RowLabels=NULL, ColLabels=NULL ) {
  #Get the matrix dimensions specified in the file
  RootAttr <- getRootAttrOMX( OMXFileName )
  Shape <- RootAttr$SHAPE
  #Define function to parse a selection statement and return corresponding data indices
  findIndex <- function( SelectionStmt ) {
    StmtParse <- unlist( strsplit( SelectionStmt, " " ) )
    IsBlank <- sapply( StmtParse, nchar ) == 0
    StmtParse <- StmtParse[ !IsBlank ]
    LookupName <- StmtParse[1]
    Lookup <- readLookupOMX( OMXFileName, LookupName )
    assign( LookupName, Lookup[[1]]  )
    which( eval( parse( text=SelectionStmt ) ) )
  }
  #Make index for row selection
  if( !is.null( RowSelection ) ) {
    RowIndex <- 1:Shape[1]
    for( Stmt in RowSelection ) {
      Index <- findIndex( Stmt )
      RowIndex <- intersect( RowIndex, Index )
      rm( Index )
    }
  } else {
    RowIndex <- NULL
  }
  #Make index for column selection
  if( !is.null( ColSelection ) ) {
    ColIndex <- 1:Shape[2]
    for( Stmt in ColSelection ) {
      Index <- findIndex( Stmt )
      ColIndex <- intersect( ColIndex, Index )
      rm( Index )
    }
  } else {
    ColIndex <- NULL
  }
  #Extract the matrix meeting the selection criteria
  Result <- readMatrixOMX( OMXFileName, MatrixName, RowIndex=RowIndex, ColIndex=ColIndex )
  #Label the rows and columns
  if( !is.null( RowLabels ) ) {
    rownames( Result ) <- readLookupOMX( OMXFileName, RowLabels )[[1]][ RowIndex ]
  }
  if( !is.null( ColLabels ) ) {
    colnames( Result ) <- readLookupOMX( OMXFileName, ColLabels )[[1]][ ColIndex ]
  }
  #Return the matrix
  Result
}
