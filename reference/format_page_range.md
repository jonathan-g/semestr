# Format a Range of Pages

Format a range of pages, accounting for both consecutive and
non-consecutive pages.

## Usage

``` r
format_page_range(pages)
```

## Arguments

- pages:

  A string describing the pages.

## Value

Formatted page range.

## Details

Figures out whether there is one page or more than one. Add the
appropriate prefix ("p." or "pp.").

## Examples

``` r
format_page_range("99")
#> [1] "p. 99"
format_page_range("72 and 103")
#> [1] "pp. 72 and 103"
format_page_range("50--75")
#> [1] "pp. 50--75"
```
