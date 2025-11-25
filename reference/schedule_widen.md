# Pivot a schedule data frame into wide format.

Combine a `schedule` data frame, containing class descriptions, reading
assignments, and homework assignments, along with other data, merge
them, and pivot to a wide format, where every calendar entry has a
column for the reading assignment, homework due that day, and other
information.

## Usage

``` r
schedule_widen(
  schedule,
  final_exams,
  semester,
  final_is_take_home = TRUE,
  create_paths = TRUE
)
```

## Arguments

- schedule:

  A `schedule` data frame, as returned from
  [`init_schedule()`](https://jonathan-g.github.io/semestr/reference/init_schedule.md).

- final_exams:

  DESCRIPTION.

- semester:

  A `semester` object (a list), as returned from
  [`load_semester_db()`](https://jonathan-g.github.io/semestr/reference/load_semester_db.md)

- final_is_take_home:

  DESCRIPTION.

- create_paths:

  DESCRIPTION.

## Value

A `schedule` data frame, converted to wide format.
