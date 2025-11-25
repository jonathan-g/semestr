# Format a range of dates

Format a range of dates.

## Usage

``` r
format_date_range(dates, abbr = TRUE)

format_day_date_range(dates, abbr_month = TRUE, abbr_wday = TRUE)
```

## Arguments

- dates:

  A named list or vector of dates with elements `start` and `stop`.

- abbr:

  Abbreviate the month.

- abbr_month:

  Abbreviate the months.

- abbr_wday:

  Abbreviate the days of the week.

## Value

A formatted date

## Details

Format a range of dates, using a similar format to
[`format_class_date()`](https://jonathan-g.github.io/semestr/reference/format_class_date.md)

## Functions

- `format_day_date_range()`: Format a range of dates, including day of
  the week.

## Examples

``` r
format_date_range(list(start = as.Date("2020-02-14"),
                       stop  = as.Date("2020-03-15")))
#> [1] "Feb. 14--Mar. 15"

format_day_date_range(list(start = as.Date("2020-02-14"),
                           stop  = as.Date("2020-03-15")))
#> [1] "Fri., Feb. 14--Sun., Mar. 15"
```
