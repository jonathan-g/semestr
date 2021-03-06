% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/text_formatting.R
\name{format_page_range}
\alias{format_page_range}
\title{Format a Range of Pages}
\usage{
format_page_range(pages)
}
\arguments{
\item{pages}{A string describing the pages.}
}
\value{
Formatted page range.
}
\description{
Format a range of pages, accounting for both consecutive and non-consecutive
pages.
}
\details{
Figures out whether there is one page or more than one. Add the appropriate
prefix ("p." or "pp.").
}
\examples{
format_page_range("99")
format_page_range("72 and 103")
format_page_range("50--75")
}
