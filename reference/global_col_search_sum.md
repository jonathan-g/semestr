# Count pattern-matches in each column of data frames

Search a list of data frames for a pattern and count the number of
matches in each column.

## Usage

``` r
global_col_search_sum(df_lst, pattern)
```

## Arguments

- df_lst:

  A named list of data frames

- pattern:

  The pattern to search for (regular expression)

## Value

A named list of data frames where each column has the number of times
that column matched the pattern.
