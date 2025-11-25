# Escape dollar sign for LaTeX

Escape single backslashes in front of dollar signs (for LaTeX)

## Usage

``` r
escape_dollar(txt)
```

## Arguments

- txt:

  A character string

## Value

A character string with escaped backslashes in front of dollar sings.

## Details

If a dollar sign in a character string is preceded by a single
backslash, escape that to make it a double-backslash. Ignore cases where
the dollar sign is already preceded by multiple backslashes.

## Examples

``` r
escape_dollar("$x^2$")
#> [1] "$x^2$"
escape_dollar("\\$1000.00")
#> [1] "\\\\$1000.00"
escape_dollar("$\\\\$1000.00")
#> [1] "$\\\\$1000.00"
```
