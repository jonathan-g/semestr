# Strip prefixes off keys.

Keys in the master calendar's `cal_key` column have prefixes according
to the type of calendar entry they represent (e.g., "`LAB_`" for labs,
"`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.).
This function strips those off.

## Usage

``` r
strip_key_prefix(x, type, ...)

# S3 method for class 'character'
strip_key_prefix(x, type, ...)

# S3 method for class 'list'
strip_key_prefix(x, type, ...)

# S3 method for class 'data.frame'
strip_key_prefix(x, type, col = "cal_key", ...)
```

## Arguments

- x:

  The data frame to process.

- type:

  The type of calendar entry to strip (e.g., "`class`", "`lab`", etc.)

- ...:

  Arguments to pass to specialized methods.

- col:

  The column where the keys are located (by default "`cal_key`").

## Value

A list or vector frame with the prefixes stripped from the contents.

A data frame with the prefixes stripped from the specified column.

## Methods (by class)

- `strip_key_prefix(character)`: Strip key prefixes from character
  vector.

- `strip_key_prefix(list)`: Strip key prefixes from a list of character
  objects.

- `strip_key_prefix(data.frame)`: Strip key prefixes from a column in a
  data frame.

## See also

add_key_prefix

add_key_prefix
