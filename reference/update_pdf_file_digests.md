# Update digests for PDF files

Update digests for all PDF files under the content path.

## Usage

``` r
update_pdf_file_digests(
  files,
  root_dir,
  static_path = "static",
  content_path = "content",
  partial = FALSE
)
```

## Arguments

- files:

  Files to update.

- root_dir:

  The root directory for the project (Should be the root for a HUGO
  project).

- static_path:

  Where to look for static files (in the HUGO sense of static).

- content_path:

  Where to look for content (e.g., `.Rmd` and `.md` files).

- partial:

  Only update digests for new files without previous digests..

## Value

A data frame with the digests.

## Details

If the `root_dir` is `NULL`, the function tries to find the root
directory using a few common heuristics.
