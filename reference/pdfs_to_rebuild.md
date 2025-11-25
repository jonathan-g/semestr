# Figure out which files need to be rebuilt

`pdfs_to_rebuild` returns a vector of files that need to be rebuilt.

## Usage

``` r
pdfs_to_rebuild(
  files,
  root_dir,
  static_path = "static",
  content_path = "content"
)
```

## Arguments

- files:

  A character vector of paths to source files (e.g., `.Rmd`).

## Value

A character vector of files that need to be rebuilt.

## Details

This function accepts a vector of source files and returns a vector of
files that need to be rebuilt because the source file is new or has
changed since the last time the site was built.

## See also

[`get_current_pdf_digests()`](https://jonathan-g.github.io/semestr/reference/get_current_pdf_digests.md),
[blogdownDigest::digests](https://jonathan-g.github.io/blogdownDigest/reference/digests.html).
