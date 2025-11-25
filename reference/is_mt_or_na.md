# Extends is.na to report TRUE if the object has length zero.

If I want to check whether an element in a list is missing, sometimes
it's `NA`, but sometimes it is simply a vector of length 0. This checks
for both possibilities.

## Usage

``` r
is_mt_or_na(x)
```

## Arguments

- x:

  A vector of any type, of length 0 or 1

## Value

TRUE if the vector has length 0 or is NA

## Examples

``` r
is_mt_or_na(character(0))
#> [1] TRUE
is_mt_or_na(NA_character_)
#> [1] TRUE
is_mt_or_na("foo")
#> [1] FALSE
```
