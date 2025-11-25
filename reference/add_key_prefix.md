# Add prefixes to keys.

Keys in the master calendar's `cal_key` column have prefixes according
to the type of calendar entry they represent (e.g., "`LAB_`" for labs,
"`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.).
This function adds those prefixes.

## Usage

``` r
add_key_prefix(x, type, ...)

# S3 method for class 'character'
add_key_prefix(x, type, ...)

# S3 method for class 'list'
add_key_prefix(x, type, ...)

# S3 method for class 'data.frame'
add_key_prefix(x, type, col = "cal_key", ...)
```

## Arguments

- x:

  The object to process.

- type:

  The type of prefix to add (e.g., "`class`", "`lab`", etc.)

- ...:

  Arguments passed to methods.

- col:

  The column to process

## Value

A data frame with the prefixes stripped from the specified column.

A data frame with the prefixes stripped from the specified column.

## Methods (by class)

- `add_key_prefix(character)`: Add key prefixes to a character vector.

- `add_key_prefix(list)`: Add key prefixes to a list of character
  objects.

- `add_key_prefix(data.frame)`: Add key prefix to a column in a data
  frame.

## See also

strip_key_prefix

strip_key_prefix
