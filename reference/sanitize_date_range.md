# Create a date range from a list of dates

Create a date range from a list of dates.

## Usage

``` r
sanitize_date_range(dates)
```

## Arguments

- dates:

  A list or vector of dates.

## Value

A named list with `start` and `stop` elements.

## Details

Take an arbitrary list of dates and create a date range from the first
to the last.

## Examples

``` r
sanitize_date_range(c(as.Date("2021-02-20"), as.Date("2021-03-15"),
                      as.Date("2019-12-15"), as.Date("2020-07-14")))
#> $start
#> [1] "2019-12-15"
#> 
#> $stop
#> [1] "2021-03-15"
#> 
```
