#Utility function to read the SHAPE and VERSION attributes
#---------------------------------------------------------
#This function reads the SHAPE and VERSION attributes of an OMX file. This is called by several other functions
#Arguments:
#OMXFileName = full path name of the OMX file being read
#Return: List containing the SHAPE  and VERSION attributes.
#SHAPE component is a vector of number of rows and columns
#VERSION component is the version number
getRootAttrOMX <- function( OMXFileName ) {
	H5File <- H5Fopen( OMXFileName )
  H5Attr <- H5Aopen( H5File, "SHAPE" )
  RootAttr <- list()
  RootAttr$SHAPE <- H5Aread( H5Attr )
  H5Aclose( H5Attr )
  H5Attr <- H5Aopen( H5File, "OMX_VERSION" )
  RootAttr$VERSION <- H5Aread( H5Attr )
  H5Aclose( H5Attr )
  H5Fclose( H5File )
  RootAttr
}



#Function to list the contents of an OMX file
#--------------------------------------------
#This function lists the contents of an omx file. These include:
#OMX version
#Matrix shape
#Names, descriptions, datatypes, and NA values of all of the matrices in an OMX file.
#Names and descriptions of indices and whether each index applies to rows, columns or both
#Arguments:
#OMXFileName = full path name of the OMX file
#Return: A list with 5 components:
#Version - the OMX version number
#Rows - number of rows in the matrix
#Columns - number of columns in the matrix
#Matrices - a dataframe identifying the matrices and all their attributes
#Lookups - a dataframe identifying the lookups and all their attributes
listOMX <- function( OMXFileName ) {
  #Get the version and shape information
	RootAttr <- getRootAttrOMX( OMXFileName )
	Version <- RootAttr$VERSION
	Shape <- RootAttr$SHAPE
  #Use the h5ls function to read the contents of the file
  Contents <- h5ls( OMXFileName )
  MatrixContents <- Contents[ Contents$group == "/data", ]
  LookupContents <- Contents[ Contents$group == "/lookup", ]
  #Read the matrix attribute information
  Names <- MatrixContents$name
  Types <- MatrixContents$dclass
  H5File <- H5Fopen( OMXFileName )
  H5Group <- H5Gopen( H5File, "data" )
  MatAttr <- list()
  for( i in 1:length(Names) ) {
    Attr <- list()
    H5Data <- H5Dopen( H5Group, Names[i] )
    if(H5Aexists(H5Data, "NA")) {
      H5Attr <- H5Aopen( H5Data, "NA" )
      Attr$navalue <- H5Aread( H5Attr )
      H5Aclose( H5Attr )
    }
    if(H5Aexists(H5Data, "Description")) {
      H5Attr <- H5Aopen( H5Data, "Description" )
      Attr$description <- H5Aread( H5Attr )
      H5Aclose( H5Attr )
    }
    MatAttr[[Names[i]]] <- Attr
    H5Dclose( H5Data )
    rm( Attr )
  }
  H5Gclose( H5Group )
  H5Fclose( H5File )
  MatAttr <- do.call( rbind, lapply( MatAttr, function(x) data.frame(x) ) )
  rm( Names, Types )
  #Read the lookup attribute information
  H5File <- H5Fopen( OMXFileName )
  H5Group <- H5Gopen( H5File, "lookup" )
  Names <- LookupContents$name
  Types <- LookupContents$dclass
  LookupAttr <- list()
  for( i in 1:length(Names) ) {
    Attr <- list()
    H5Data <- H5Dopen( H5Group, Names[i] )
    if( H5Aexists( H5Data, "DIM" ) ) {
      H5Attr <- H5Aopen( H5Data, "DIM" )
      Attr$lookupdim <- H5Aread( H5Attr )
      H5Aclose( H5Attr )
    } else {
      Attr$lookupdim <- ""
    }
    if( H5Aexists( H5Data, "Description" ) ) {
      H5Attr <- H5Aopen( H5Data, "Description" )
      Attr$description <- H5Aread( H5Attr )
      H5Aclose( H5Attr )
    } else {
      Attr$description <- ""
    }
    LookupAttr[[Names[i]]] <- Attr
    H5Dclose( H5Data )
    rm( Attr )
  }
  H5Gclose( H5Group )
  H5Fclose( H5File )
  LookupAttr <- do.call( rbind, lapply( LookupAttr, function(x) data.frame(x) ) )
  rm( Names, Types )
  #Combine the results into a list
  if(length(MatAttr)>0) {
    MatInfo <- cbind( MatrixContents[,c("name","dclass","dim")], MatAttr )
  } else {
    MatInfo <- MatrixContents[,c("name","dclass","dim")]
  }
  LookupInfo <- cbind( LookupContents[,c("name","dclass","dim")], LookupAttr )
  list( OMXVersion=Version, Rows=Shape[1], Columns=Shape[2], Matrices=MatInfo, Lookups=LookupInfo )
}

