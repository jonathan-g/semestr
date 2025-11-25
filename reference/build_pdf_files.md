# Build PDF versions of assignments.

Build PDF versions of assignments RMarkdown.

## Usage

``` r
build_pdf_files(
  semester,
  content_path = "content",
  static_path = "static",
  force_dest = TRUE,
  output_options = NULL
)
```

## Arguments

- semester:

  A list of data returned from `\link{load_semester_db}`

- content_path:

  A relative path to the HUGO content directory.

- static_path:

  Relative path to the HUGO static directory.

- force_dest:

  Create any missing directories.

- output_options:

  A list of extra output options for `\link{pdf_document}`.

## Value

A vector of paths to the resulting PDF files.
