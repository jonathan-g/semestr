# Look Up Past and Future Classes

Look up previous or future classes, based on the current class's id.

## Usage

``` r
lookup_future_class(calendar, id, delta = 1)

lookup_past_class(calendar, id, delta = 1)
```

## Arguments

- calendar:

  The calendar to use for looking up the classes.

- id:

  The `cal_id` index of the current class.

- delta:

  The number of classes forward or backward.

## Value

A row from the calendar table.

## Functions

- `lookup_future_class()`:

- `lookup_past_class()`:
