% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cross_referencing.R
\name{lookup_past_future_reading}
\alias{lookup_past_future_reading}
\alias{lookup_future_reading}
\alias{lookup_past_reading}
\title{Look Up Past and Future Reading Assignments}
\usage{
lookup_future_reading(schedule, id = NULL, grp = NULL, delta = 1)

lookup_past_reading(schedule, id = NULL, grp = NULL, delta = 1)
}
\arguments{
\item{schedule}{The schedule to use for looking up the classes.}

\item{id}{The \code{cal_id} index of the current class.}

\item{grp}{The \code{rd_grp_key} for the current class's reading group.}

\item{delta}{The number of classes forward or backward.}
}
\value{
A row from the calendar table.
}
\description{
Look up previous or future assignments, based on the current class's id.
}
\section{Functions}{
\itemize{
\item \code{lookup_future_reading()}: 

\item \code{lookup_past_reading()}: 

}}
