# Determine the modification type of calendar entry from its calendar id.

Modifications include canceled and re-scheduled (make-up) classes.

## Usage

``` r
item_mod(cal_id)
```

## Arguments

- cal_id:

  an integer calendar ID number.

## Value

A string identifying the type of modification. Current values are
"canceled" and "make-up"
