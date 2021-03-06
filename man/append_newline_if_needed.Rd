% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_formatting.R
\name{append_newline_if_needed}
\alias{append_newline_if_needed}
\title{Make sure a string ends with a newline.}
\usage{
append_newline_if_needed(
  txt,
  start_par = FALSE,
  extra_lines = 0,
  collapse = NULL
)
}
\arguments{
\item{txt}{A vector of character strings.}

\item{start_par}{Add a newline at the beginning of the string.
If there is more than one string in \code{txt}, \code{start_par} should have
length 1 or the length of \code{txt}.}

\item{extra_lines}{The number of blank lines to follow the string.
If there is more than one string in \code{txt}, \code{extra_lines} should have
length 1 or the length of \code{txt}.}

\item{collapse}{Collapse all strings in \code{txt} into a single string after
appending newlines. This can bee logical, in which case the collapsed
strings are separated by an empty string, or it can be a character string.}
}
\value{
A vector of character strings.
}
\description{
Append newlines to the end of strings.
}
\details{
Take a character vector, ensure that each element ends in a newline,
Optionally add an extra newline to the beginning of each element, add
extra blank lines at the end, and optionally concatenate the elements
together into a single string, separated by an optional separator in
addition to the new lines.
}
\examples{
append_newline_if_needed("foo")
append_newline_if_needed("foo", start_par = TRUE)
append_newline_if_needed("foo", extra_lines = 3)
append_newline_if_needed(month.abb, start_par = c(TRUE, rep(FALSE, 11)),
  collapse = TRUE)
append_newline_if_needed(month.abb, collapse = "...")
append_newline_if_needed(c("Months:", month.abb, extra_lines = 0,
  collapse = "* "))

}
