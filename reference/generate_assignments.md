# Generate assignments from database

Generate RMarkdown files for reading, homework, and lab assignments and
a lessons.yml file for Hugo to use in making a schedule for a course.

## Usage

``` r
generate_assignments(semester)
```

## Arguments

- semester:

  A semester object returned from
  [`load_semester_db()`](https://jonathan-g.github.io/semestr/reference/load_semester_db.md).

## Value

A named list containing the lesson plan in YAML text format and the
semester schedule, as a
[`tibble`](https://tibble.tidyverse.org/reference/tbl_df-class.html).

## Examples

``` r
if (FALSE) { # \dontrun{
sem <- load_semester_db("foo.sqlite3")
asgts <- generate_assignments(sem)
} # }
```
