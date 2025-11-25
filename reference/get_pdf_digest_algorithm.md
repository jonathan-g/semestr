# Get the digest algorithm to use

`get_pdf_digest_algorithm` gets the digest algorithm that will be used.

## Usage

``` r
get_pdf_digest_algorithm()
```

## Value

A string containing the name of the algorithm.

## Details

Set the algorithm with `options(blogdown.hash.algorithm = <algorithm>)`.
If the option is not set, then use crc32.

## See also

[blogdownDigest::digests](https://jonathan-g.github.io/blogdownDigest/reference/digests.html).
