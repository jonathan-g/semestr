# Make a Reading Assignment Page

Make a page for the reading assignment for one class, from the semester
database.

## Usage

``` r
make_reading_page(cal_id, semester, schedule, use_pdfs = TRUE)
```

## Arguments

- cal_id:

  The calendar ID for this assignment.

- semester:

  A semester object (a list returned by `\link{load_semester_db}`).

- schedule:

  A data frame with the semester schedule (returned by
  [prepare_schedule](https://jonathan-g.github.io/semestr/reference/prepare_schedule.md))

- use_pdfs:

  Add a `pdf_url` field to the YAML header so a PDF file will be
  generated for the reading assignment.

## Value

A character string with the Markdown content for the reading assignment
page.
