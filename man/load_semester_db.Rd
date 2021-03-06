% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_semester_db.R
\name{load_semester_db}
\alias{load_semester_db}
\title{Load schedule for semester from database}
\usage{
load_semester_db(db_file, root_crit = NULL, ignore_root = FALSE)
}
\arguments{
\item{db_file}{An SQLite database file.}

\item{root_crit}{Criteria for \code{rprojroot} to use in finding the project
root directory.}

\item{ignore_root}{Ignore the root criteria and work from the current
directory.}
}
\value{
A list containing a tibbles with the calendar for the semester and
tibbles with details on reading assignments, homework assignments,
labs, exams, holidays, and other events.

The list contains the following tibbles:
\code{calendar}, \code{due_dates}, \code{rd_items}, \code{rd_src},
\code{class_topics}, \code{hw_asgt}, \code{hw_items}, \code{hw_sol}, \code{hw_topics},
\code{lab_asgt}, \code{lab_items}, \code{lab_sol}, \code{exams},
\code{holidays}, \code{events}, \code{notices},
\code{has_reading}, \code{has_homework}, \code{has_labs}, \code{has_exams},
\code{has_holidays}, \code{has_events}, \code{has_notices},
\code{text_codes},
\code{metadata}, \code{semester_dates}, \code{tz}, \code{root_dir}, \code{slide_dir}

and a named list \code{metadata} containing named character vectors of
metadata that are used to decode and manipulate calendar entries.
}
\description{
Loads schedule for class meetings, reading and homework assignments,
lab sessions, exams, holidays, etc. from a database.
}
