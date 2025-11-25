# Formatting parts of dates

Format the month or day of week for a date.

## Usage

``` r
format_month(d, abbr = TRUE)

format_wday(d, abbr = TRUE)
```

## Arguments

- d:

  A date

- abbr:

  Abbreviate the name.

## Value

The name of the month or day of the week.

## Functions

- `format_month()`: Format the month

- `format_wday()`: Format the day of the week

## Examples

``` r
format_month(as.Date("2001-02-25"), FALSE)
#> [1] "February"
format_month(Sys.Date(), TRUE)
#> [1] "Nov."

format_wday(as.Date("2001-02-25"), FALSE)
#> [1] Sunday
#> 7 Levels: Sunday < Monday < Tuesday < Wednesday < Thursday < ... < Saturday
format_wday(Sys.Date(), TRUE)
#> [1] "Tue."
```
