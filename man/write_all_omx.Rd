% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_write_omx.R
\name{write_all_omx}
\alias{write_all_omx}
\title{Write all matrix cores to an OMX file}
\usage{
write_all_omx(object, file, long = TRUE)
}
\arguments{
\item{object}{The object to be written to OMX. May be either a tibble or 
a list of named matrices.}

\item{file}{The location of the OMX file.}

\item{long}{Identify whether the object to be written is a long-format 
`tibble` (the default, `TRUE`) or a list of named matrices.}
}
\value{
List of write function return codes; 0 if successful.
}
\description{
Write all matrix cores to an OMX file
}
\examples{
omxfile <- omxr_example("skims.omx")
# long-format (tibble)
skims <- read_all_omx(omxfile, names = c("DIST", "DISTBIKE", "DISTWALK"))
write_all_omx(skims, file = tempfile(fileext = ".omx"))

# list of matrices
skims <- read_all_omx(omxfile, names = c("DIST", "DISTBIKE", "DISTWALK"), long = FALSE)
write_all_omx(skims, file = tempfile(fileext = ".omx"), long = FALSE)

}
