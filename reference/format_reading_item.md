# Format a Reading Item

Takes a reading item and formats it.

## Usage

``` r
format_textbook_reading_item(reading_item)

format_handout_reading_item(
  reading_item,
  online_location = getOption("semestr.online_reading_loc")
)

format_web_reading_item(
  reading_item,
  online_location = getOption("semestr.online_reading_loc")
)

format_youtube_reading_item(
  reading_item,
  online_location = getOption("semestr.online_reading_loc")
)
```

## Arguments

- reading_item:

  The reading item (a row from the semester `rd_items` data frame)

- online_location:

  An URL for where to find the video.

## Value

A character string with the formatted output.

## Functions

- `format_textbook_reading_item()`: Format reading from a textbook.

- `format_handout_reading_item()`: Format a handout reading item.

- `format_web_reading_item()`: Format a web page reading item.

- `format_youtube_reading_item()`: Format a YouTube video.
