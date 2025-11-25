# Look up and format a range of dates

Look up dates from the semester schedule and format them as a range from
earliest to latest.

## Usage

``` r
format_date_range_by_cal_id(
  calendar,
  cal_ids,
  abbr = TRUE,
  days = FALSE,
  abbr_wday = NULL
)

format_date_range_by_class_num(
  calendar,
  nums,
  abbr = TRUE,
  days = FALSE,
  abbr_wday = NULL
)

format_date_range_by_key(
  calendar,
  keys,
  type = c("class", "reading", "lab", "homework", "exam", "holiday", "event", "due date",
    "raw"),
  abbr = TRUE,
  days = FALSE,
  abbr_wday = NULL
)

format_date_range_by_event_id(
  calendar,
  event_ids,
  abbr = TRUE,
  days = FALSE,
  abbr_wday = NULL
)
```

## Arguments

- calendar:

  The calendar to use for looking up the date.

- cal_ids:

  A list or vector of calendar IDs.

- abbr:

  Abbreviate the month.

- days:

  Include day of week in formatted date.

- abbr_wday:

  Abbreviate weekdays (if this is `NULL`, use `abbr`).

- nums:

  A list or vector of class numbers.

- keys:

  A list or vector of key indices to look up.

- type:

  Which type of key is this ("class", "reading", "lab", "homework",
  "exam", "holiday", "event", "due date", or "raw")

- event_ids:

  A list or vector of event ids.

## Value

A formatted date range

## Details

If the key type is `"raw"`, then the fully formatted key should be used
(e.g., "CLS_INTRO" for a class key). Otherwise, the prefix will be added
automatically based on the type, so `key = "INTRO", type = "lab"` will
become `LAB_INTRO`.

## Functions

- `format_date_range_by_cal_id()`: Look up dates by calendar ID.

- `format_date_range_by_class_num()`: Look up dates by class number.

- `format_date_range_by_key()`: Look up dates by key

- `format_date_range_by_event_id()`: Look up dates by event id.
