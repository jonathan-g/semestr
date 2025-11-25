# Format a date

Format a date as month and day.

## Usage

``` r
format_class_date(d, abbr = TRUE)

format_class_day_date(d, abbr_month = TRUE, abbr_wday = TRUE)
```

## Arguments

- d:

  A date.

- abbr:

  Abbreviate the month.

- abbr_month:

  Abbreviate the month.

- abbr_wday:

  Abbreviate the day of the week.

## Value

The formatted date.

## Functions

- `format_class_day_date()`: Format a date as day of week, month, and
  day.

## Examples

``` r
format_class_date(Sys.Date())
#> [1] "Nov. 25"

format_class_day_date(Sys.Date())
#> [1] "Tue., Nov. 25"
```
