% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_formatting.R
\name{sanitize_date_range}
\alias{sanitize_date_range}
\title{Create a date range from a list of dates}
\usage{
sanitize_date_range(dates)
}
\arguments{
\item{dates}{A list or vector of dates.}
}
\value{
A named list with \code{start} and \code{stop} elements.
}
\description{
Create a date range from a list of dates.
}
\details{
Take an arbitrary list of dates and create a date range from the first to the
last.
}
\examples{
sanitize_date_range(c(as.Date("2021-02-20"), as.Date("2021-03-15"),
                      as.Date("2019-12-15"), as.Date("2020-07-14")))

}
