# Strip the final exams from a schedule

Remove the final exams from a semester schedule.

## Usage

``` r
schedule_strip_finals(schedule, semester)
```

## Arguments

- schedule:

  A schedule data frame, as returned from
  [`init_schedule()`](https://jonathan-g.github.io/semestr/reference/init_schedule.md).

- semester:

  A semester object (a list), returned from
  [`load_semester_db()`](https://jonathan-g.github.io/semestr/reference/load_semester_db.md).

## Value

A list of a `schedule` data frame with the exams removed, and a
`final_examss` data frame containing the final exams rows.
