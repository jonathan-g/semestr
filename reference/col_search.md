# Search for a pattern in any column of a data frame

Search every character column of a data frame for a pattern.

## Usage

``` r
col_search(df, pattern)
```

## Arguments

- df:

  A data frame

- pattern:

  The pattern to search for (regular expression)

## Value

A data frame where each character column is replaced with "HIT" if it
matches the pattern and "" if it doesn't.
