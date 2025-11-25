# Get the time zone for where the course will be taught.

The semester planning database should contain information about the time
zone where the course will be taught. This function retrieves the time
zone corresponding to the currently loaded semester schedule.

## Usage

``` r
get_semestr_tz()
```

## Value

A character string with the time zone.

## Examples

``` r
get_semestr_tz()
#> [1] "America/Chicago"
```
