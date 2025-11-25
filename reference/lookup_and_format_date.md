# Look Up and Format a Date

Look up a date from the semester schedule and format it.

## Usage

``` r
format_date_by_cal_id(calendar, id, abbr = TRUE)

format_date_by_class_num(calendar, num, abbr = TRUE)

format_date_by_key(
  calendar,
  key,
  type = c("class", "reading", "lab", "homework", "exam", "holiday", "event", "due date",
    "raw"),
  abbr = TRUE
)

format_day_date_by_cal_id(calendar, id, abbr_month = TRUE, abbr_wday = TRUE)

format_day_date_by_class_num(
  calendar,
  num,
  abbr_month = TRUE,
  abbr_wday = TRUE
)

format_day_date_by_key(
  calendar,
  key,
  type = c("class", "reading", "lab", "homework", "exam", "holiday", "event", "due date",
    "raw"),
  abbr_month = TRUE,
  abbr_wday = TRUE
)
```

## Arguments

- calendar:

  The calendar to use for looking up the date.

- id:

  The `cal_id` index to look up.

- abbr:

  Abbreviate the month.

- num:

  The class session number to look up.

- key:

  The value to look up.

- type:

  Which index to use in the lookup ("class", "reading", "lab",
  "Homework", "exam", "holiday", "event", "due date", or "raw".. '

- abbr_month:

  Abbreviate the month.

- abbr_wday:

  Abbreviate the day of the week.

## Value

A formatted date.

## Details

If the key type is `"raw"`, then the fully formatted key should be used
(e.g., "CLS_INTRO" for a class key). Otherwise, the prefix will be added
automatically based on the type, so `key = "INTRO", type = "lab"` will
become `LAB_INTRO`.

## Functions

- `format_date_by_cal_id()`: Look up a date by calendar id

- `format_date_by_class_num()`: Look up a date by class session

- `format_date_by_key()`: Look up a date by arbitrary key.

- `format_day_date_by_cal_id()`: Look up a date by calendar id.

- `format_day_date_by_class_num()`: Look up a date by class session.

- `format_day_date_by_key()`: Look up a date by arbitrary key.

## Examples

``` r
if (FALSE) { # \dontrun{
format_date_by_key(calendar, "IR_RADIATION", "class")
} # }

if (FALSE) { # \dontrun{
format_date_by_key(calendar, "IR_RADIATION", "class")
} # }

if (FALSE) { # \dontrun{
format_day_date_by_class_num(calendar, 11)
} # }
```
