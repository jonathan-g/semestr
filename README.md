semestr
================
Jonathan Gilligan
2022-08-31

<!-- README.md is generated from README.Rmd. Please edit that file -->

# semestr

<!-- badges: start -->

[![Version:
0.2.1](https://img.shields.io/github/r-package/v/jonathan-g/semestr?label=version)](https://github.com/jonathan-g/semestr/releases/tag/v0.2.1)
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/semestr)](https://CRAN.R-project.org/package=semestr)
[![License:
MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![R-CMD-check](https://github.com/jonathan-g/semestr/workflows/R-CMD-check/badge.svg)](https://github.com/jonathan-g/semestr/actions)
<!-- badges: end -->

The goal of semestr is to manage a syllabus for a university course. The
package uses a SQL database with a schedule of reading, homework, and
laboratory assignments and due dates, exams, holidays, etc. There are
functions for generating content for a HUGO-based web site with the
semester schedule and the assignents, as well as PDF copies of the
assignments and a full syllabus.

## Status

This project is at a very early state of development and is not ready
for general use. Use this at your own risk.

## Installation

This package has not been released to CRAN. You can install the
development version from GitHub using the `remotes` package:

``` r
remotes::install_github("jonathan-g/semestr")
```

## Example

To get started, you need a SQL database. I have not yet written
instructions or specifications for this database, but there is an
example database `planning/ees_3310/example_semester_db.sqlite3` and you
can inspect it and figure out how it works.

``` r
library(semestr)
semester <- load_semester_db("planning/example_semester_db.sqlite3")
generate_assignments(semester)
build_pdf_files(semester)
```
