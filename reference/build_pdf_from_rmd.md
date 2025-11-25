# Build a PDF from an RMarkdown source file

Build a PDF from an RMarkdown source file

## Usage

``` r
build_pdf_from_rmd(
  source_file,
  root_dir,
  static_path = "static",
  force_dest = FALSE,
  output_options = NULL
)
```

## Arguments

- source_file:

  The RMarkdown source file

- root_dir:

  The root directory of the HUGO project.

- static_path:

  Relative path to the HUGO static directory.

- force_dest:

  Create any missing directories.

- output_options:

  A list of extra output options for `\link{pdf_document}`.

## Value

The path to the resulting PDF file.
