# Add a Period If Necessary

Add a period at the end of a sentence if necessary.

## Usage

``` r
add_period(str)
```

## Arguments

- str:

  A string, ending in a sentence.

## Value

A string ending in punctuation.

## Details

Trim any whitespace at the end, and if the line does not end with
punctuation, add a period.

## Examples

``` r
add_period("This needs a period")
#> [1] "This needs a period."
add_period("This does not need another period.")
#> [1] "This does not need another period."
add_period("Fix spaces at the end   ")
#> [1] "Fix spaces at the end."
add_period("Shouldn't I omit a period if the sentence already ends with other punctuation?  ")
#> [1] "Shouldn't I omit a period if the sentence already ends with other punctuation?"
```
