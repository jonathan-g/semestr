# Look Up Past and Future Reading Assignments

Look up previous or future assignments, based on the current class's id.

## Usage

``` r
lookup_future_reading(schedule, id = NULL, grp = NULL, delta = 1)

lookup_past_reading(schedule, id = NULL, grp = NULL, delta = 1)
```

## Arguments

- schedule:

  The schedule to use for looking up the classes.

- id:

  The `cal_id` index of the current class.

- grp:

  The `rd_grp_key` for the current class's reading group.

- delta:

  The number of classes forward or backward.

## Value

A row from the calendar table.

## Functions

- `lookup_future_reading()`:

- `lookup_past_reading()`:
