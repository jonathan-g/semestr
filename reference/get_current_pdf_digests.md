# Create a data frame with stored digests and digests of current files

`get_current_pdf_digests` returns a data frame with a row for every file
and columns for stored and current digests of source and output files.

## Usage

``` r
get_current_pdf_digests(
  files,
  root_dir = NULL,
  static_path = "static",
  content_path = "content"
)
```

## Arguments

- files:

  A character vector of paths to source files (e.g., `.Rmd`).

## Value

A a data frame with a row for every file and columns:

- `file`:

  The source file name.

- `dest`:

  The output file name.

- `alg`:

  The digest algorithm.

- `digest`:

  The stored digest for the source file.

- `dest_digest`:

  The stored digest for the output file.

- `cur_digest`:

  The digest for the current source file.

- `cur_dest_digest`:

  The digest for the current output file.

Digests for missing files are set to `NA`.

## Details

This function accepts a vector of source files and returns a data frame
with a row for each file and columns for the stored digests and the
digests of current source and output files.

## See also

[`pdfs_to_rebuild()`](https://jonathan-g.github.io/semestr/reference/pdfs_to_rebuild.md),
[`pdf_digest_if_exists()`](https://jonathan-g.github.io/semestr/reference/pdf_digest_if_exists.md),
[blogdownDigest::digests](https://jonathan-g.github.io/blogdownDigest/reference/digests.html).
