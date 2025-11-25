# Load schedule for semester from database

Loads schedule for class meetings, reading and homework assignments, lab
sessions, exams, holidays, etc. from a database.

## Usage

``` r
load_semester_db(db_file, root_crit = NULL, ignore_root = FALSE)
```

## Arguments

- db_file:

  An SQLite database file.

- root_crit:

  Criteria for `rprojroot` to use in finding the project root directory.

- ignore_root:

  Ignore the root criteria and work from the current directory.

## Value

A list containing a tibbles with the calendar for the semester and
tibbles with details on reading assignments, homework assignments, labs,
exams, holidays, and other events.

The list contains the following tibbles: `calendar`, `due_dates`,
`rd_items`, `rd_src`, `handouts`, `class_topics`, `hw_asgt`, `hw_items`,
`hw_sol`, `hw_topics`, `lab_asgt`, `lab_items`, `lab_sol`, `exams`,
`holidays`, `events`, `notices`, `has_reading`, `has_handouts`,
`has_homework`, `has_labs`, `has_exams`, `has_holidays`, `has_events`,
`has_notices`, `file_paths`, `text_codes`, `metadata`, `semester_dates`,
`tz`, `root_dir`, `slide_dir`

and a named list `metadata` containing named character vectors of
metadata that are used to decode and manipulate calendar entries.
