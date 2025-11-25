# Extract the local PDF filename corresponding to an URL.

Extract the path to a local PDF file corresponding to an URL on the web
site.

## Usage

``` r
pdf_filename(
  pdf_url,
  root_dir,
  static_path = "static",
  force_dest = FALSE,
  ignore_missing_dest = FALSE,
  verbose = FALSE
)
```

## Arguments

- pdf_url:

  The URL (in the destination web site).

- root_dir:

  The root directory of the local HUGO project.

- static_path:

  Relative path to the HUGO static files.

- force_dest:

  Create missing directories?

- ignore_missing_dest:

  Ignore missing directories.

- verbose:

  Provide informative messages and warnings.

## Value

The filename
