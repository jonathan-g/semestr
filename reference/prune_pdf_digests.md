# Delete stored digests for specified source files

`prune_pdf_digests` removes the lines from the digest file corresponding
to a vector of source files.

## Usage

``` r
prune_pdf_digests(files, root_dir = NULL)
```

## Arguments

- files:

  A character vector of paths to the source files to be removed.

- root_dir:

  The root directory of the HUGO project.

## Value

The path to the digest file.

## Details

Modifies the stored digest file to remove lines corresponding to
selected source files.

## See also

[`blogdownDigest::update_site_digests()`](https://jonathan-g.github.io/blogdownDigest/reference/update_site_digests.html),
[blogdownDigest::digests](https://jonathan-g.github.io/blogdownDigest/reference/digests.html).
