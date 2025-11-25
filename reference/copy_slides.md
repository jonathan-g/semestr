# Install Powerpoint slides for a class session

For class sessions where a Powerpoint slide deck will be used instead of
`reveal.js` slides, copy the Powerpoint slides from a main directory to
the class slide directory.

## Usage

``` r
copy_slides(schedule, date, cal_entry, semester)
```

## Arguments

- schedule:

  A schedule data frame.

- date:

  The reading due date.

- cal_entry:

  A calendar entry for the class.

- semester:

  A list of data for the semester, from the database.

## Value

An updated schedule data frame

## Details

Normally, `semestr` assumes that the user will be using `reveal.js`, and
that the slides will be installed (e.g., by using `gulp sync` from the
directory where the slides are authored), but sometimes, especially for
guest lectures, I use Powerpoint, and this lets me copy files from a
main directory where I save Powerpoint slides into the `/slides/`
directory for the website.
