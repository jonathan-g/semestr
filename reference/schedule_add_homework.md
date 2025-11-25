# Add homework assignments to a schedule

Add homework assignments to a schedule data frame

## Usage

``` r
schedule_add_homework(schedule, semester)
```

## Arguments

- schedule:

  A `schedule` data frame, as returned from
  [`init_schedule()`](https://jonathan-g.github.io/semestr/reference/init_schedule.md).

- semester:

  A `semester` object (a list), as returned from
  [`load_semester_db()`](https://jonathan-g.github.io/semestr/reference/load_semester_db.md)

## Value

A `schedule` data frame, with the homework assignments added.
