% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pdf_digests.R
\name{get_current_pdf_digests}
\alias{get_current_pdf_digests}
\title{Create a data frame with stored digests and digests of current files}
\usage{
get_current_pdf_digests(
  files,
  root_dir = NULL,
  static_path = "static",
  content_path = "content"
)
}
\arguments{
\item{files}{A character vector of paths to source files (e.g., \code{.Rmd}).}
}
\value{
A a data frame with a row for every file and columns:
\describe{
\item{\code{file}}{The source file name.}
\item{\code{dest}}{The output file name.}
\item{\code{alg}}{The digest algorithm.}
\item{\code{digest}}{The stored digest for the source file.}
\item{\code{dest_digest}}{The stored digest for the output file.}
\item{\code{cur_digest}}{The digest for the current source file.}
\item{\code{cur_dest_digest}}{The digest for the current output file.}
}

Digests for missing files are set to \code{NA}.
}
\description{
\code{get_current_pdf_digests} returns a data frame with a row for every file
and columns for stored and current digests of source and output files.
}
\details{
This function accepts a vector of source files and
returns a data frame with a row for each file and columns for the
stored digests and the digests of current source and output files.
}
\seealso{
\code{\link[=pdfs_to_rebuild]{pdfs_to_rebuild()}},
\code{\link[=pdf_digest_if_exists]{pdf_digest_if_exists()}}, \link[blogdownDigest:digests]{blogdownDigest::digests}.
}
\keyword{internal}
