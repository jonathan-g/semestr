# Search data frames for a pattern in any column

Search data frames and return lines where any column matches a pattern.

## Usage

``` r
global_col_search(df_lst, pattern)
```

## Arguments

- df_lst:

  A named list of data frames

- pattern:

  The pattern to search for (regular expression)

## Value

A named list of data frames where character columns are replaced with
"HIT" for matches and "" for misses.
