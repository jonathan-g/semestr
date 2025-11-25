# Calculate digest for pdf file

Check whether a PDF file exists, and if so calculate its digest.

## Usage

``` r
pdf_digest_if_exists(file, alg = NA_character_)
```

## Arguments

- file:

  The pdf file (the file need not exist).

- alg:

  The digest algorithm to use.

## Value

A character vector with the digest, or `NA` if the file does not exist.
