# Check which files need to be rebuilt

`pdf_needs_rebuild` returns a vector of logicals indicating which files
need to be rebuilt, based on whether the file has changed.

## Usage

``` r
pdf_needs_rebuild(
  current_digest,
  current_dest_digest,
  old_digest,
  old_dest_digest
)
```

## Arguments

- current_digest:

  A character vector containing digests of the current source files
  (`.Rmd` or `.rmarkdown`\`).

- current_dest_digest:

  A character vector containing digests of the current destination
  (output) files (`.html`\`). `NA` for destination files that do not
  exist.

- old_digest:

  The stored digest for the source file from the last time the site was
  built. `NA` if the source file did not exist at the time of the last
  build.

- old_dest_digest:

  A character vector containing stored digests for the destination files
  from the last time the site was built. `NA` for destination files that
  did not exist after the last build.

## Value

A vector of logicals indicating whether the destination (output) files
are out of date relative to the source files.

If a destination file is missing or if any of the digests don't match,
then the file needs to be rebuilt.

## Details

This function compares digests of current files to stored digests in
order to tell whether the source file needs to be rebuilt. If the
digests are not equal, then the file has changed. If a digest is
missing, then the source file is new or the output file has been deleted
and in either case, the source file needs to be rebuilt.

## See also

[blogdownDigest::digests](https://jonathan-g.github.io/blogdownDigest/reference/digests.html).
