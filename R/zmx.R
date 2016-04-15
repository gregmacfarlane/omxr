#' Convert ZMX to OMX
#'
#' @param zmx Path to \code{.zmx} file.
#' @param omx Optional path to new omx file. If \code{NULL}, will save in same
#'   directory with same name as \code{zmx}.
#'
#'
#' @author Ben Stabler
#'
#' @export
#'
convert_zmx = function(zmx, omx = NULL) {
  x = read_zmx(zmx)

  # set output file name
  if(is.null(omx)){
    outFName = gsub(".zmx", ".omx", zmx)
  } else {
    outFName = omx
  }

  matName = gsub(".zmx", "", zmx)
  create_omx(outFName, nrow(x), ncol(x), 7)
  write_omx(outFName, x, matName)
  write_lookup(outFName, rownames(x), "NO")
}

#' Read a PB ZMX file
#'
#' @param zmx Path to a ZMX file.
#' @return An R matrix object.
#'
#' @details The ZMX was a PB-designed precursor to OMX. This function is
#'   provided for backwards compatibility only.
#'
#' @author Ben Stabler, Brian Gregor
#'
#' @export
#'
read_zmx <- function(zmx) {

  #Make a temporary directory to put unzipped files into
  dir.create( "TempZip" )

  #Read matrix attributes
  NumRows = as.integer(
    scan( utils::unzip( zmx, "_rows", exdir="TempZip" ), what="", quiet=T ) )
  NumCols = as.integer(
    scan( utils::unzip( zmx, "_columns", exdir="TempZip" ), what="", quiet=T ) )
  RowNames = strsplit(
    scan( utils::unzip( zmx, "_external row numbers", exdir="TempZip" ),
          what="", quiet=T ),"," )[[1]]
  ColNames = strsplit(
    scan( utils::unzip( zmx, "_external column numbers", exdir="TempZip" ),
          what="", quiet=T ),"," )[[1]]

  #Initialize matrix to hold values
  Result.ZnZn = matrix( 0, NumRows, NumCols )
  rownames( Result.ZnZn ) = RowNames
  colnames( Result.ZnZn ) = ColNames

  #Read matrix data by row and place in initialized matrix
  RowDataEntries. = paste("row_", 1:NumRows, sep="")
  for(i in 1:NumRows){
    Result.ZnZn[ i, ] = readBin(unzip(
      zmx, RowDataEntries.[i], exdir="TempZip" ),
      what=double(), n=NumCols, size=4, endian="big" )
  }

  #Remove the temporary directory
  unlink( "TempZip", recursive=TRUE )

  #Return the matrix
  Result.ZnZn
}


#' Write a PB ZMX file
#'
#' @param Matrix An R matrix
#' @param zmx Path to a ZMX file.
#' @param sevenz Path to 7zip executable. Defaults to \code{SEVENZIP_BIN} in
#'   \code{Sys.getenv()}
#' @return An R matrix object.
#'
#' @details The ZMX was a PB-designed precursor to OMX. This function is
#'   provided for backwards compatibility only.
#'
#' @author Ben Stabler, Brian Gregor
#'
#' @export
#'
write_zmx <- function(Matrix, zmx, sevenz = NULL) {

  if(is.null(sevenz)){
    sevenz = Sys.getenv("SEVENZIP_BIN")
  }

  #Make a temporary directory to put unzipped files into
  tempDir = tempdir()
  print(tempDir)
  oldDir = getwd()
  setwd(tempDir)

  #Write matrix attributes
  cat(2, file="_version")
  cat(zmx, file="_name")
  cat(zmx, file="_description")

  cat(nrow(Matrix), file="_rows")
  cat(ncol(Matrix), file="_columns")
  cat(paste(rownames(Matrix),collapse=","), file="_external row numbers")
  cat(paste(colnames(Matrix),collapse=","), file="_external column numbers")

  #Write rows
  for(i in 1:nrow(Matrix)) {
    writeBin(Matrix[i,], paste("row_", i, sep=""), size=4, endian="big")
  }

  #Create file
  filesToInclude = normalizePath(dir(tempDir, full.names=T))
  filesToInclude = paste(paste('"', filesToInclude, '"\n', sep=""), collapse=" ")
  listzmx = paste(tempDir, "/listfile.txt", sep="")
  write(filesToInclude, listzmx)
  setwd(oldDir)
  command = paste(sevenz, " a -tzip ", zmx, " @", listzmx, sep="")
  system(command)
}
