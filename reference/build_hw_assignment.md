# Build a homework assignment

Build a homework assignment: Generate an `.Rmd` file for a homework
assignment web page and PDF handout.

## Usage

``` r
build_hw_assignment(schedule, date, cal_entry, semester)
```

## Arguments

- schedule:

  A schedule data frame.

- date:

  The homework due date.

- cal_entry:

  A calendar entry for the homework due date.

- semester:

  A list of data for the semester, from the database.

## Value

An updated schedule data frame
