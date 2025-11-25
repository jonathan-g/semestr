# Make sure a string ends with a newline.

Append newlines to the end of strings.

## Usage

``` r
append_newline_if_needed(
  txt,
  start_par = FALSE,
  extra_lines = 0,
  collapse = NULL
)
```

## Arguments

- txt:

  A vector of character strings.

- start_par:

  Add a newline at the beginning of the string. If there is more than
  one string in `txt`, `start_par` should have length 1 or the length of
  `txt`.

- extra_lines:

  The number of blank lines to follow the string. If there is more than
  one string in `txt`, `extra_lines` should have length 1 or the length
  of `txt`.

- collapse:

  Collapse all strings in `txt` into a single string after appending
  newlines. This can bee logical, in which case the collapsed strings
  are separated by an empty string, or it can be a character string.

## Value

A vector of character strings.

## Details

Take a character vector, ensure that each element ends in a newline,
Optionally add an extra newline to the beginning of each element, add
extra blank lines at the end, and optionally concatenate the elements
together into a single string, separated by an optional separator in
addition to the new lines.

## Examples

``` r
append_newline_if_needed("foo")
#> [1] "foo\n"
append_newline_if_needed("foo", start_par = TRUE)
#> [1] "\nfoo\n"
append_newline_if_needed("foo", extra_lines = 3)
#> [1] "foo\n\n\n\n"
append_newline_if_needed(month.abb, start_par = c(TRUE, rep(FALSE, 11)),
  collapse = TRUE)
#> [1] "\nJan\nFeb\nMar\nApr\nMay\nJun\nJul\nAug\nSep\nOct\nNov\nDec\n"
append_newline_if_needed(month.abb, collapse = "...")
#> [1] "Jan\n...Feb\n...Mar\n...Apr\n...May\n...Jun\n...Jul\n...Aug\n...Sep\n...Oct\n...Nov\n...Dec\n"
append_newline_if_needed(c("Months:", month.abb, extra_lines = 0,
  collapse = "* "))
#>                                                                         
#> "Months:\n"     "Jan\n"     "Feb\n"     "Mar\n"     "Apr\n"     "May\n" 
#>                                                                         
#>     "Jun\n"     "Jul\n"     "Aug\n"     "Sep\n"     "Oct\n"     "Nov\n" 
#>             extra_lines    collapse 
#>     "Dec\n"       "0\n"       "*\n" 
```
