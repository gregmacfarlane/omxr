#' Function to write a lookup vector to an OMX file
#'
#' This function writes a lookup vector to the file. It allows the user to
#' specify if the lookup vector applies only to rows or columns (in case the
#' matrix is not square and/or the rows and columns don't have the same
#' meanings.
#'
#' @param file Full path name of the OMX file to store the lookup.
#' @param lookup_v Vector containing the lookup information. Should be the same
#'   length as the matrix dimension it maps to (see \code{lookup_dim})
#' @param name Name for the lookup vector in \code{file}.
#' @param lookup_dim Matrix dimension that the lookup vector is associated with
#'   Values can be "row", "col", or NULL, meaning that the lookup is
#'   two-dimensional. Defaults to \code{NULL}.
#' @param replace Overwrite the existing lookup (if any)? Defaults to
#'   \code{FALSE}.
#' @param description String defining the lookup.
#' 
#' @importFrom rhdf5 h5write H5Fopen H5Gopen H5Dopen h5writeAttribute H5Dclose
#'   H5Gclose H5Fclose
#'
#' @export
#' @examples 
#' \dontrun{
#' omxfile <- omxr_example("skims.omx")
#' EI <- c(rep("Int", 21), rep("Ext", 4))
#' D <-  rep(1:5, each = 5)
#' write_lookup(omxfile, EI, "EI", description = "External and Internal Zones")
#' write_lookup(omxfile, D, "Districts", description = "Districts")
#' }
write_lookup <- function(file, lookup_v, name,
                         lookup_dim = NULL, replace = FALSE, description="" ) {

  #Check whether there is lookup of that name already in the file
  H5File <- rhdf5::H5Fopen( file )
  Contents = rhdf5::h5ls(H5File)
  LookupNames <- Contents$name[ Contents$group == "/lookup" ]
  if((name %in% LookupNames) & !replace){
    rhdf5::H5Fclose( H5File )
    stop(paste(
      name, "already exists. Set replace = TRUE to overwrite."
    ))
  }

  #Error check lookup dimension arguments
  Len <- length(lookup_v)
  RootAttr <- get_omx_attr( file )
  Shape <- RootAttr$SHAPE

  if(is.null(lookup_dim)) {  # no dimension given, meaning two-way lookup
    if( Shape[1] != Shape[2] ) {
      rhdf5::H5Fclose( H5File )
      stop("Matrix is not square. You must specify the 'lookup_dim'")
    }
    if(Len != Shape[1]) {
      rhdf5::H5Fclose( H5File )
      stop(paste(
        file, "has", Shape[1], "rows and columns. lookup_v has",
        Len, " positions."
      ))
    }

  } else {  # dimension is given

    # needs to be either row or col
    if(!(lookup_dim %in% c( "row", "col" ))) {
      rhdf5::H5Fclose( H5File )
      stop("lookup_dim argument must be 'row', 'col', or NULL.")
    }

    # check length
    if(lookup_dim == "row" & Len != Shape[1]){
      rhdf5::H5Fclose( H5File )
      stop(paste("Length of 'lookup_v' does not match row dimension of", file))
    }
    if(lookup_dim == "col" & Len != Shape[2]){
      rhdf5::H5Fclose( H5File )
      stop(paste("Length of 'lookup_v' does not match row dimension of", file))
    }
  }

  #Write lookup vector to file
  ItemName <- paste( "lookup", name, sep="/" )
  rhdf5::h5writeDataset.logical( lookup_v, H5File, ItemName )
  #Write attributes

  H5Group <- rhdf5::H5Gopen( H5File, "lookup" )
  H5Data <- rhdf5::H5Dopen( H5Group, name )
  rhdf5::h5writeAttribute.character( description, H5Data, "Description" )
  if( !is.null( lookup_dim ) ) {
    rhdf5::h5writeAttribute.character( lookup_dim, H5Data, "DIM" )
  }

  #Close everything up before exiting
  rhdf5::H5Dclose( H5Data )
  rhdf5::H5Gclose( H5Group )
  rhdf5::H5Fclose( H5File )
}


#' Function to read an OMX lookup
#'
#' This function reads a lookup and its attributes.
#'
#' @param file  Path name of the OMX file where the lookup resides.
#' @param name Name of the lookup in the OMX file
#'
#' @return A list of two elements:
#'  \describe{
#'    \item{\code{lookup}}{The lookup vector.}
#'    \item{\code{lookup_dim}}{String, whether the lookup refers to rows or
#'      columns. }
#'  }
#' @importFrom rhdf5 h5read H5Fopen H5Gopen H5Dopen H5Aexists H5Aopen H5Aread
#'   H5Aclose H5Dclose H5Gclose H5Fclose
#' @export
#' @examples
#' omxfile <- omxr_example("skims.omx")
#' read_lookup(omxfile, "EI")
#' 
read_lookup <- function( file, name ) {
  #Identify the item to be read
  ItemName <- paste( "lookup", name, sep="/" )
  #Read the lookup
  Lookup <- rhdf5::h5read( file, ItemName )
  #Read the name of the dimension the lookup corresponds
  H5File <- rhdf5::H5Fopen( file )
  H5Group <- rhdf5::H5Gopen( H5File, "lookup" )
  H5Data <- rhdf5::H5Dopen( H5Group, name )
  if( rhdf5::H5Aexists( H5Data, "DIM" ) ) {
    H5Attr <- rhdf5::H5Aopen( H5Data, "DIM" )
    Dim <- rhdf5::H5Aread( H5Attr )
    rhdf5::H5Aclose( H5Attr )
  } else {
    Dim <- ""
  }

  rhdf5::H5Dclose( H5Data )
  rhdf5::H5Gclose( H5Group )
  rhdf5::H5Fclose( H5File )

  #Return the lookup and the corresponding dimension
  list( Lookup=Lookup, lookup_dim=Dim )
}

#' Read a OMX matrix based on a lookup vector
#'
#' This function reads a portion of an OMX matrix using selection statements to
#' define the portion.
#'
#' @details  Multiple selection selection statements can be used for
#' each dimension. Each selection statement is a logical expression represented
#' in a double-quoted string. The left operand is the name of a lookup vector
#' The operator can be any logical operator including %in% The right operand is
#' the value or values to check against. This can be the name of a vector
#' defined in the calling environment. If the right operand contains literal
#' string values, those values must be single-quoted. Multiple selection
#' conditions may be used as argument by including in a vector. Multiple
#' selection conditions are treated as intersections.
#'
#' @param file Path name of the OMX file where the lookup resides.
#' @param matrix_name Name of the matrix in the OMX file.
#' @param row_selection Row selection statement or vector of row selection
#'   statements (see Details).
#' @param col_selection Column selection statement or vector of column selection
#'   statements (see Details).
#' @param row_labels Name of lookup to use for labeling rows.
#' @param col_labels Name of lookup to use for labeling columns
#'
#' @return An R matrix object representing the selected rows and colums.
#'
#' @export
#' @examples
#' omxfile <- omxr_example("skims.omx")
#' read_selected_omx(
#'   omxfile, "DIST", row_selection = 'EI == "Ext"' 
#' )
#' 
read_selected_omx <- function(file, matrix_name,
                              row_selection = NULL, col_selection = NULL,
                              row_labels = NULL, col_labels = NULL) {

  #Get the matrix dimensions specified in the file
  RootAttr <- get_omx_attr( file )
  Shape <- RootAttr$SHAPE

  # function to parse a selection statement and return corresponding indices
  findIndex <- function( SelectionStmt ) {
    StmtParse <- unlist( strsplit( SelectionStmt, " " ) )
    IsBlank <- sapply( StmtParse, nchar ) == 0
    StmtParse <- StmtParse[ !IsBlank ]
    LookupName <- StmtParse[1]
    Lookup <- read_lookup( file, LookupName )
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
  Result <- read_omx(file, matrix_name,
                     row_index = row_index, col_index = col_index)

  #Label the rows and columns
  if( !is.null( row_labels ) ) {
    rownames( Result ) <- read_lookup( file, row_labels )[[1]][ row_index ]
  }
  if( !is.null( col_labels ) ) {
    colnames( Result ) <- read_lookup( file, col_labels )[[1]][ col_index ]
  }

  Result
}
