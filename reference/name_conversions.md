# Metadata Name Conversion Functions

Convert between different types of metadata representing categories of
possible calendar entries.

## Usage

``` r
type2col(type)

type2idx(type)

type2prefix(type)

type2base(type)

idx2col(idx)

idx2type(idx)

col2idx(col)

col2type(col)

base2type(base)
```

## Arguments

- type:

  A type (character)

- idx:

  An index (character)

- col:

  A column prefix/suffix (character)

- base:

  An integer base.#'

## Value

A character or integer value corresponding to the request.

## Details

The kinds of metadata include

- **type**: A type of calendar entry, in text. "class", "lab",
  "homework", "due date", "exam", "holiday", and "event".

- **idx**: Indices for entries in vectors and lists: "class",
  "homework", "due_date", "exam", "holiday", "event", "notice".

- **col**: A string used as prefix or suffix for a data frame column:
  "class", "lab", "hw", "due", "exam", "holiday", "event", "notice".

- **prefix**: A prefix used to distinguish calendar keys: "CLS\_",
  "LAB\_", "HW\_", "DUE\_", "EXAM\_", "VAC\_", and "EVT\_".

- **base**: An integer value used to identify numeric calendar entries.
  Multiples of 1000.

## Functions

- `type2col()`: Convert a type to a column name.

- `type2idx()`: Convert a type to an index.

- `type2prefix()`: Convert a type to a prefix.

- `type2base()`: Convert a type to a base.

- `idx2col()`: Convert an index to a column.

- `idx2type()`: Convert an index to a type.

- `col2idx()`: Convert a column to an index.

- `col2type()`: Convert a column to a type.

- `base2type()`: Convert a base to a type.
