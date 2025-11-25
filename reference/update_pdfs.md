# Update all files that are out of date

`update_pdfs` rebuilds all source files that are new or have changed
since the last time the site was built.

## Usage

``` r
update_pdfs(
  dir = NULL,
  root_dir = NULL,
  static_path = "static",
  content_path = "content",
  quiet = FALSE,
  force = FALSE,
  force_dest = TRUE,
  output_options = NULL
)
```

## Arguments

- dir:

  A string containing the root directory for checking. By default, the
  "content" directory of the project.

- root_dir:

  The root directory of the HUGO project.

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

- output_options:

  A list of extra output options for `\link{pdf_document}`.

## Value

This function does not return anything

## Details

Given a source directory (by default the "content" directory in the root
directory of the project), find all source files (`.Rmd` and
`.rmarkdown`) in the directory tree under the source directory,
calculate hashed digests of the files, and compare them to a stored list
of digests from the last time the site was built.

If the digests of either the source or output files don't match, if a
source file is new since the last time the site was built, or if the
output file does not exist, then render the source file.

After rendering any out-of-date files, regenerate the digest list and
saves it to a file.

## See also

[`build_site()`](https://pkgs.rstudio.com/blogdown/reference/build_site.html),
[`build_dir()`](https://pkgs.rstudio.com/blogdown/reference/build_dir.html),
[blogdownDigest::digests](https://jonathan-g.github.io/blogdownDigest/reference/digests.html).
