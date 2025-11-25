# Build a reading assignment

Build a reading assignment: Generate an `.Rmd` file for a reading
assignment web page and PDF handout.

## Usage

``` r
build_reading_assignment(schedule, date, cal_entry, semester)
```

## Arguments

- schedule:

  A schedule data frame.

- date:

  The reading due date.

- cal_entry:

  A calendar entry for the class.

- semester:

  A list of data for the semester, from the database.

## Value

An updated schedule data frame
