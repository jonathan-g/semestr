# Count the number of times each column in a data frame matches a pattern.

Search every character column of a data frame for a pattern and count
hits.

## Usage

``` r
col_search_sum(df, pattern, na.rm = TRUE)
```

## Arguments

- df:

  A data frame

- pattern:

  The pattern to search for (regular expression)

- na.rm:

  Ignore missing values.

## Value

A data frame where each column is the number of times that column
matched the target pattern.
