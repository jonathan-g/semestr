# Generates and stores digests for all source and output files.

[`blogdownDigest::update_site_digests`](https://jonathan-g.github.io/blogdownDigest/reference/update_site_digests.html)
calculates hashed digests for a site.

## Usage

``` r
update_pdf_digests(
  dir = NULL,
  root_dir = NULL,
  static_path = "static",
  content_path = "content",
  partial = FALSE
)
```

## Arguments

- dir:

  A string with the name of the directory to search (by default the
  "content" directory at the top-level directory of the site) If the
  `root_dir` is `NULL`, the function tries to find the root directory
  using a few common heuristics.

- root_dir:

  The root directory for the project (Should be the root for a HUGO
  project).

- static_path:

  Where to look for static files (in the HUGO sense of static).

- content_path:

  Where to look for content (e.g., `.Rmd` and `.md` files).

- partial:

  Logical. If `TRUE`, keep digests for source files that aren't in the
  specified directory and its children and descendants. Otherwise, get
  rid of the old digest file and only keep digests for source files in
  the source directory and its descendants.

## Value

The path to the digest file.

## Details

Generates new hashed digests for both source and destination (output)
files and save the digests to a file "`digests.Rds`" in the root
directory of the site.

## See also

[`prune_pdf_digests()`](https://jonathan-g.github.io/semestr/reference/prune_pdf_digests.md),
[`update_site()`](https://jonathan-g.github.io/blogdownDigest/reference/update_site.html),
[blogdownDigest::digests](https://jonathan-g.github.io/blogdownDigest/reference/digests.html).
