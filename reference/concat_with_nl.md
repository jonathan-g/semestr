# Concatenate strings as separate lines of text.

Combine strings, as separate lines of text.

## Usage

``` r
concat_with_nl(s, ..., start_par = FALSE, extra_lines = 0, collapse = "")
```

## Arguments

- s:

  A character string

- ...:

  Additional character strings

- start_par:

  Put an extra newline between each pair of strings, so each string
  starts a new paragraph.

- extra_lines:

  Number of extra newlines to append after each string.

- collapse:

  Extra characters to insert between the strings.

## Value

A character string.

## Details

Take two or more lines of text. Make sure they each end with a newline
and then concatenate them.

## Examples

``` r
concat_with_nl("foo", "bar")
#> [1] "foo\nbar\n"
concat_with_nl("foo", "bar", start_par = TRUE)
#> [1] "foo\n\nbar\n"
concat_with_nl("foo", "bar", collapse = "...")
#> [1] "foo\n...bar\n"
```
