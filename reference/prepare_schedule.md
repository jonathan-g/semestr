# Prepare schedule from database

Prepare schedule from database

## Usage

``` r
prepare_schedule(semester)
```

## Arguments

- semester:

  A semester object returned from
  [`load_semester_db()`](https://jonathan-g.github.io/semestr/reference/load_semester_db.md).

## Value

A tibble containing a schedule

## Examples

``` r
if (FALSE) { # \dontrun{
sem <- load_semester_db("foo.sqlite3")
sched <- prepare_schedule(sem)
} # }
```
