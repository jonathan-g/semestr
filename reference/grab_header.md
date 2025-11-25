# Extract metadata from an RMD file.

Extract metadata from the YAML header in an RMD file.

## Usage

``` r
grab_header(f, buflines = 20)
```

## Arguments

- f:

  The filename

- buflines:

  How big a chunk to read in at a time.

## Value

A data frame containing the header metadata.
