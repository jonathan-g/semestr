# Format a Collection of Reading Items

Takes a collection of reading items and formats them.

## Usage

``` r
format_textbook_reading(reading_list)

format_handout_reading(reading_list)

format_web_reading(reading_list)

format_youtube_reading(reading_list)
```

## Arguments

- reading_list:

  A data frame with rows corresponding to reading items (a subset from
  the semester `rd_items` data frame)

## Value

A character vector with the formatted output for each item.

## Functions

- `format_textbook_reading()`: Format a collection of textbook readings.

- `format_handout_reading()`: Format reading from multiple handouts.

- `format_web_reading()`: Format reading from multiple web pages.

- `format_youtube_reading()`: Format reading from multiple YouTube
  videos.
