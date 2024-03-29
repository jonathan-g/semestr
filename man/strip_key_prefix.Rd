% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/key_prefixes.R
\name{strip_key_prefix}
\alias{strip_key_prefix}
\alias{strip_key_prefix.character}
\alias{strip_key_prefix.list}
\alias{strip_key_prefix.data.frame}
\title{Strip prefixes off keys.}
\usage{
strip_key_prefix(x, type, ...)

\method{strip_key_prefix}{character}(x, type, ...)

\method{strip_key_prefix}{list}(x, type, ...)

\method{strip_key_prefix}{data.frame}(df, type, col = "cal_key")
}
\arguments{
\item{x}{An object to process.}

\item{type}{The type of calendar entry to strip (e.g., "\code{class}", "\code{lab}",
etc.)}

\item{...}{Arguments to pass to specialized methods.}

\item{df}{The data frame to process.}

\item{col}{The column where the keys are located (by default "\code{cal_key}").}
}
\value{
A list or vector frame with the prefixes stripped from the contents.

A data frame with the prefixes stripped from the specified column.
}
\description{
Keys in the master calendar's \code{cal_key} column have prefixes according to
the type of calendar entry they represent (e.g., "\code{LAB_}" for labs,
"\code{CLS_}" for classes/reading assignments, "\code{EXAM_}" for exams, etc.). This
function strips those off.
}
\section{Methods (by class)}{
\itemize{
\item \code{strip_key_prefix(character)}: Strip key prefixes from character vector.

\item \code{strip_key_prefix(list)}: Strip key prefixes from a list of character objects.

\item \code{strip_key_prefix(data.frame)}: Strip key prefixes from a column in a data frame.

}}
\seealso{
add_key_prefix

add_key_prefix
}
