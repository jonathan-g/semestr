# Rebuild changed files in a subdirectory of "content"

`update_dir` updates changed files in a subdirectory of "content"

## Usage

``` r
update_pdf_dir(
  dir = ".",
  root_dir = NULL,
  static_path = "static",
  content_path = "content",
  quiet = TRUE,
  force = FALSE,
  force_dest = FALSE,
  ignore = NA,
  output_options = NULL
)
```

## Arguments

- dir:

  A string containing the root directory for checking. By default, the
  "content" directory of the project.

- root_dir:

  The root directory for the project (Should be the root for a HUGO
  project).

- static_path:

  Where to look for static files (in the HUGO sense of static).

- content_path:

  Where to look for content (e.g., `.Rmd` and `.md` files).

- quiet:

  Suppress output. By default this is `FALSE` and the function emits an
  informational message about how many files will be rebuilt.

- force:

  Force rebuilding source files that are not out of date.

- force_dest:

  Create missing destination directories.

- ignore:

  A regular expression pattern for files to ignore.

- output_options:

  A list of extra output options for `\link{pdf_document}`.

## Value

A list of out-of-date files to rebuild.
