#' Read OMX attributes
#'
#' This function reads the SHAPE and VERSION attributes of an OMX file. This is
#' called by several other functions.
#'
#' @param file full path name of the OMX file being read
#' @return A list containing \code{SHAPE} and \code{VERSION} attributes.
#'
#' @export
#' @import rhdf5
get_omx_attr <- function( file ) {
	H5File <- rhdf5::H5Fopen( file )
  H5Attr <- rhdf5::H5Aopen( H5File, "SHAPE" )
  RootAttr <- list()
  RootAttr$SHAPE <- rhdf5::H5Aread( H5Attr )
  rhdf5::H5Aclose( H5Attr )
  H5Attr <- rhdf5::H5Aopen( H5File, "OMX_VERSION" )
  RootAttr$VERSION <- rhdf5::H5Aread( H5Attr )
  rhdf5::H5Aclose( H5Attr )
  rhdf5::H5Fclose( H5File )

  RootAttr
}



#' List the contents of an OMX file
#'
#' @param file The path to the OMX file.
#'
#' @return A list with 5 elements:
#' \describe{
#'   \item{Version}{the OMX version number}
#'   \item{Rows}{The number of rows in the matrix.}
#'   \item{Columns}{The number of columns in the matrix.}
#'   \item{Matrices}{A \code{data.frame} identifying the matrices and their
#'   attributes}
#'   \item{Lookups}{A \code{data.frame} identifying the lookups and their
#'   attributes.}
#'  }
#'
#' @export
#' @import rhdf5
list_omx <- function( file ) {
  #Get the version and shape information
	RootAttr <- getRootAttrOMX( file )
	Version <- RootAttr$VERSION
	Shape <- RootAttr$SHAPE
  #Use the h5ls function to read the contents of the file
  Contents <- rhdf5::h5ls( file )
  MatrixContents <- Contents[ Contents$group == "/data", ]
  LookupContents <- Contents[ Contents$group == "/lookup", ]
  #Read the matrix attribute information
  Names <- MatrixContents$name
  Types <- MatrixContents$dclass
  H5File <- rhdf5::H5Fopen( file )
  H5Group <- rhdf5::H5Gopen( H5File, "data" )
  MatAttr <- list()

  for( i in 1:length(Names) ) {
    Attr <- list()
    H5Data <- H5Dopen( H5Group, Names[i] )
    if(H5Aexists(H5Data, "NA")) {
      H5Attr <- rhdf5::H5Aopen( H5Data, "NA" )
      Attr$navalue <- rhdf5::H5Aread( H5Attr )
      rhdf5::H5Aclose( H5Attr )
    }
    if(H5Aexists(H5Data, "Description")) {
      H5Attr <- rhdf5::H5Aopen( H5Data, "Description" )
      Attr$description <- rhdf5::H5Aread( H5Attr )
      rhdf5::H5Aclose( H5Attr )
    }
    MatAttr[[Names[i]]] <- Attr
    rhdf5::H5Dclose( H5Data )
    rm( Attr )
  }

  rhdf5::H5Gclose( H5Group )
  rhdf5::H5Fclose( H5File )

  MatAttr <- do.call( rbind, lapply( MatAttr, function(x) data.frame(x) ) )
  rm( Names, Types )

  #Read the lookup attribute information
  H5File <- rhdf5::H5Fopen( file )
  H5Group <- rhdf5::H5Gopen( H5File, "lookup" )
  Names <- LookupContents$name
  Types <- LookupContents$dclass
  LookupAttr <- list()
  for( i in 1:length(Names) ) {
    Attr <- list()
    H5Data <- rhdf5::H5Dopen( H5Group, Names[i] )
    if( rhdf5::H5Aexists( H5Data, "DIM" ) ) {
      H5Attr <- rhdf5::H5Aopen( H5Data, "DIM" )
      Attr$lookupdim <- rhdf5::H5Aread( H5Attr )
      rhdf5::H5Aclose( H5Attr )
    } else {
      Attr$lookupdim <- ""
    }
    if( rhdf5::H5Aexists( H5Data, "Description" ) ) {
      H5Attr <- rhdf5::H5Aopen( H5Data, "Description" )
      Attr$description <- rhdf5::H5Aread( H5Attr )
      rhdf5::H5Aclose( H5Attr )
    } else {
      Attr$description <- ""
    }

    LookupAttr[[Names[i]]] <- Attr
    rhdf5::H5Dclose( H5Data )
    rm( Attr )
  }

  rhdf5::H5Gclose( H5Group )
  rhdf5::H5Fclose( H5File )

  LookupAttr <- do.call( rbind, lapply( LookupAttr, function(x) data.frame(x) ) )
  rm( Names, Types )

  #Combine the results into a list
  if(length(MatAttr)>0) {
    MatInfo <- cbind( MatrixContents[,c("name","dclass","dim")], MatAttr )
  } else {
    MatInfo <- MatrixContents[,c("name","dclass","dim")]
  }
  LookupInfo <- cbind( LookupContents[,c("name","dclass","dim")], LookupAttr )

  list(
    OMXVersion=Version,
    Rows=Shape[1], Columns=Shape[2],
    Matrices=MatInfo, Lookups=LookupInfo
  )
}

