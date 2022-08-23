#' Make sure a string ends with a newline.
#'
#' Append newlines to the end of strings.
#'
#' Take a character vector, ensure that each element ends in a newline,
#' Optionally add an extra newline to the beginning of each element, add
#' extra blank lines at the end, and optionally concatenate the elements
#' together into a single string, separated by an optional separator in
#' addition to the new lines.
#'
#'
#' @param txt A vector of character strings.
#' @param start_par Add a newline at the beginning of the string.
#'   If there is more than one string in `txt`, `start_par` should have
#'   length 1 or the length of `txt`.
#' @param extra_lines The number of blank lines to follow the string.
#'   If there is more than one string in `txt`, `extra_lines` should have
#'   length 1 or the length of `txt`.
#' @param collapse Collapse all strings in `txt` into a single string after
#'   appending newlines. This can bee logical, in which case the collapsed
#'   strings are separated by an empty string, or it can be a character string.
#'
#' @return A vector of character strings.
#'
#' @examples
#' append_newline_if_needed("foo")
#' append_newline_if_needed("foo", start_par = TRUE)
#' append_newline_if_needed("foo", extra_lines = 3)
#' append_newline_if_needed(month.abb, start_par = c(TRUE, rep(FALSE, 11)),
#'   collapse = TRUE)
#' append_newline_if_needed(month.abb, collapse = "...")
#' append_newline_if_needed(c("Months:", month.abb, extra_lines = 0,
#'   collapse = "* "))
#'
#' @export
append_newline_if_needed <- function(txt, start_par = FALSE, extra_lines = 0,
                                     collapse = NULL) {
  # txt <- stringr::str_trim(txt)
  txt <- stringr::str_trim(txt, "right")
  txt[stringr::str_detect(txt, '[^\n]$')] <- stringr::str_c(txt, '\n')
  if (length(start_par) > 1) {
    assertthat::assert_that(length(txt) == length(start_par),
                            msg = stringr::str_c("txt has length ", length(txt),
                                                 " and start_par has length ",
                                                 length(start_par)))
    txt <- stringr::str_c(ifelse(start_par, "\n", ""), txt)

  } else if (start_par) {
    txt <- stringr::str_c("\n", txt)
  }
  if(length(extra_lines) > 1) {
    assertthat::assert_that(length(txt) == length(extra_lines),
                            msg = stringr::str_c("txt has length ", length(txt),
                                                 " and extra_lines has length ",
                                                 length(extra_lines)))
    txt <- stringr::str_c(txt,
                          purrr::map_chr(extra_lines,
                                         ~stringr::str_c(ifelse(.x > 0,
                                                                rep("\n", .x), ""),
                                                         collapse = "")))
  } else if (extra_lines > 0) {
    txt <- stringr::str_c(txt, stringr::str_c(rep('\n', extra_lines),
                                              collapse = ''))
  }
  if (! is.null(collapse)) {
    if (isTRUE(collapse)) {
      collapse <- ""
    }
    txt <- stringr::str_c(txt, collapse = collapse)
  }
  txt
}

adj_nl <- append_newline_if_needed


#' Concatenate strings as separate lines of text.
#'
#' Combine strings, as separate lines of text.
#'
#' Take two or more lines of text. Make sure they each end with a newline and
#' then concatenate them.
#'
#' @param s A character string
#' @param ... Additional character strings
#' @param start_par Put an extra newline between each pair of strings, so each
#'   string starts a new paragraph.
#' @param extra_lines Number of extra newlines to append after each string.
#' @param collapse Extra characters to insert between the strings.
#'
#' @return A character string.
#' @examples
#' concat_with_nl("foo", "bar")
#' concat_with_nl("foo", "bar", start_par = TRUE)
#' concat_with_nl("foo", "bar", collapse = "...")
#'
#' @export
concat_with_nl <- function(s, ..., start_par = FALSE, extra_lines = 0,
                           collapse = "") {
  dots <- list(...)
  s <- append_newline_if_needed(s, FALSE, 0, TRUE)
  s2 <- append_newline_if_needed(as.character(unlist(dots)),
                                 start_par, extra_lines, collapse)
  if (stringr::str_starts(s2, stringr::fixed("\n")) &&
      stringr::str_ends(s, stringr::fixed("\n\n"))) {
    s <- s %>% stringr::str_replace_all("\n+$", "\n")
  }
  stringr::str_c(s, s2, sep = collapse)
}

cat_nl <- concat_with_nl


#' Escape "\\$" for LaTeX
#'
#' Escape single backslashes in front of dollar signs (for LaTeX)
#'
#' If a dollar sign in a character string is preceded by a single backslash,
#' escape that to make it a double-backslash. Ignore cases where the dollar
#' sign is already preceded by multiple backslashes.
#'
#' @param txt A character string
#'
#' @return A character string with escaped backslashes in front of dollar sings.
#' @examples
#' escape_dollar("$x^2$")
#' escape_dollar("\\$1000.00")
#' escape_dollar("$\\\\$1000.00")
#'
#' @export
escape_dollar <- function(txt) {
  stringr::str_replace_all(txt, c("([^\\\\])\\\\\\$" = "\\1\\\\\\\\$",
                                  "^\\\\\\$" = "\\\\\\\\$"))
}


#' Formatting parts of dates
#'
#' Format the month or day of week for a date.
#'
#' @param d A date
#' @param abbr Abbreviate the name.
#'
#' @return The name of the month or day of the week.
#'
#' @name date_parts
NULL

#' @describeIn date_parts Format the month
#'
#' @examples
#' format_month(as.Date("2001-02-25"), FALSE)
#' format_month(Sys.Date(), TRUE)
#'
#' @export
format_month <- function(d, abbr = TRUE) {
  m <- lubridate::month(d, label = TRUE, abbr = abbr)
  m <- as.character(m)
  if (abbr) m <- stringr::str_c(m, '.')
  m
}

#' @describeIn date_parts Format the day of the week
#'
#' @examples
#' format_wday(as.Date("2001-02-25"), FALSE)
#' format_wday(Sys.Date(), TRUE)
#'
#' @export
format_wday <- function(d, abbr = TRUE) {
  wd <- lubridate::wday(d, label = TRUE, abbr = abbr)
  if (abbr) wd <- stringr::str_c(wd, '.')
  wd
}

#' Format a date
#'
#' Format a date as month and day.
#'
#' @param d A date.
#' @param abbr Abbreviate the month.
#'
#' @return The formatted date.
#' @examples
#' format_class_date(Sys.Date())
#'
#' @export
format_class_date <- function(d, abbr = TRUE) {
  stringr::str_c(format_month(d, abbr = abbr), " ", lubridate::day(d))
}

#' @describeIn format_class_date Format a date as day of week, month, and day.
#'
#' @param abbr_month Abbreviate the month.
#' @param abbr_wday Abbreviate the day of the week.
#'
#' @examples
#' format_class_day_date(Sys.Date())
#'
#' @export
format_class_day_date <- function(d, abbr_month = TRUE, abbr_wday = TRUE) {
  stringr::str_c(format_wday(d, abbr_wday), ", ",
                 format_month(d, abbr_month), " ", lubridate::day(d))
}

#' Format a range of dates
#'
#' Format a range of dates.
#'
#' Format a range of dates, using a similar format to
#' [format_class_date()]
#'
#' @param dates A named list or vector of dates with elements `start` and `stop`.
#' @param abbr Abbreviate the month.
#'
#' @return A formatted date
#' @examples
#' format_date_range(list(start = as.Date("2020-02-14"),
#'                        stop  = as.Date("2020-03-15")))
#'
#' @export
format_date_range <- function(dates, abbr = TRUE) {
  if (is.vector(dates)) {
    dates <- as.list(dates)
  }
  with(dates, {
    output <- format_class_date(start, abbr)
    if (start != stop) {
      output <- stringr::str_c(output, '--',
                               ifelse(lubridate::month(stop) ==
                                        lubridate::month(start),
                                      lubridate::day(stop),
                                      format_class_date(stop, abbr)))
    }
    output
  })
}


#' @describeIn format_date_range Format a range of dates, including day of the week.
#'
#' @param abbr_month Abbreviate the months.
#' @param abbr_wday Abbreviate the days of the week.
#'
#' @examples
#' format_day_date_range(list(start = as.Date("2020-02-14"),
#'                            stop  = as.Date("2020-03-15")))
#'
#' @export
format_day_date_range<- function(dates, abbr_month = TRUE, abbr_wday = TRUE) {
  with(dates, {
    output <- format_class_day_date(start, abbr_month, abbr_wday)
    if (start != stop) {
      output <- stringr::str_c(output, '--',
                               format_class_day_date(stop, abbr_month,
                                                     abbr_wday))
    }
    output
  })
}

#' Look Up and Format a Date
#'
#' Look up a date from the semester schedule and format it.
#'
#' @param calendar The calendar to use for looking up the date.
#' @param abbr Abbreviate the month.
#'
#' @return A formatted date.
#'
#' @name lookup_and_format_date
NULL

#' @describeIn lookup_and_format_date Look up a date by calendar id
#'
#' @param id The `cal_id` index to look up.
#'
#' @export
format_date_by_cal_id <- function(calendar, id, abbr = TRUE) {
  d <- calendar %>% dplyr::filter(.data$cal_type == "class", .data$cal_id == id)
  format_class_date(d$date, abbr)
}


#' @describeIn lookup_and_format_date Look up a date by class session
#'
#' @param num The class session number to look up.
#'
#' @export
format_date_by_class_num <- function(calendar, num, abbr = TRUE) {
  d <- calendar %>% dplyr::filter(.data$cal_type == "class", .data$class_num == num)
  format_class_date(d$date, abbr)
}


#' @describeIn lookup_and_format_date Look up a date by arbitrary key.
#'
#' @details If the key type is `"raw"`, then the fully formatted key should
#'   be used (e.g., "CLS_INTRO" for a class key). Otherwise, the prefix will
#'   be added automatically based on the type, so `key = "INTRO", type = "lab"`
#'   will become `LAB_INTRO`.
#'
#' @param key The value to look up.
#' @param type Which index to use in the lookup ("class", "reading", "lab",
#'   "Homework", "exam", "holiday", "event", "due date", or "raw"..
#''
#' @examples
#' \dontrun{
#' format_date_by_key(calendar, "IR_RADIATION", "class")
#' }
#'
#' @export
format_date_by_key <- function(calendar, key,
                               type = c("class", "reading", "lab", "homework",
                                        "exam", "holiday", "event", "due date",
                                        "raw"),
                               abbr = TRUE) {
  type <- match.arg(type)
  if (type == "reading") type <- "class"
  if (type != "raw") {
    key <- add_key_prefix(key, type)
  }

  d <- calendar %>% dplyr::filter(.data$cal_key == key)
  format_class_date(d$date, abbr)
}

#' @describeIn lookup_and_format_date Look up a date by calendar id.
#'
#' @param abbr_month Abbreviate the month.
#' @param abbr_wday Abbreviate the day of the week.
#'
#' @examples
#' \dontrun{
#' format_date_by_key(calendar, "IR_RADIATION", "class")
#' }
#'
#' @export
format_day_date_by_cal_id <- function(calendar, id, abbr_month = TRUE,
                                      abbr_wday = TRUE) {
  d <- calendar %>% dplyr::filter(.data$cal_type == "class", .data$cal_id == id)
  format_class_day_date(d$date, abbr_month, abbr_wday)
}


#' @describeIn lookup_and_format_date Look up a date by class session.
#'
#' @examples
#' \dontrun{
#' format_day_date_by_class_num(calendar, 11)
#' }
#' @export
format_day_date_by_class_num <- function(calendar, num, abbr_month = TRUE,
                                         abbr_wday = TRUE) {
  d <- calendar %>% dplyr::filter(.data$cal_type == "class", .data$class_num == num)
  format_class_day_date(d$date, abbr_month, abbr_wday)
}


#' @describeIn lookup_and_format_date Look up a date by arbitrary key.
#'
#' @export
format_day_date_by_key <- function(calendar, key,
                                   type = c("class", "reading", "lab",
                                            "homework", "exam", "holiday",
                                            "event", "due date", "raw"),
                                   abbr_month = TRUE, abbr_wday = TRUE) {
  type <- match.arg(type)
  if (type == "reading") type <- "class"
  if (type != "raw") {
    key <- add_key_prefix(key, type)
  }

  d <- calendar %>% dplyr::filter(.data$cal_key == key)
  format_class_day_date(d$date, abbr_month, abbr_wday)
}



#' Create a date range from a list of dates
#'
#' Create a date range from a list of dates.
#'
#' Take an arbitrary list of dates and create a date range from the first to the
#' last.
#'
#' @param dates A list or vector of dates.
#'
#' @return A named list with `start` and `stop` elements.
#' @examples
#' sanitize_date_range(c(as.Date("2021-02-20"), as.Date("2021-03-15"),
#'                       as.Date("2019-12-15"), as.Date("2020-07-14")))
#'
#' @export
sanitize_date_range <- function(dates) {
  start <- min(dates, na.rm = TRUE)
  stop <- max(dates, na.rm = TRUE)
  list(start = start, stop = stop)
}

#' Look up and format a range of dates
#'
#' Look up dates from the semester schedule and format them as a range from
#' earliest to latest.
#'
#' @param calendar The calendar to use for looking up the date.
#' @param abbr Abbreviate the month.
#' @param days Include day of week in formatted date.
#' @param abbr_wday Abbreviate weekdays (if this is `NULL`, use `abbr`).
#'
#' @return A formatted date range
#'
#' @name lookup_and_format_date_range
NULL


#' @describeIn lookup_and_format_date_range Look up dates by calendar ID.
#'
#' @param cal_ids A list or vector of calendar IDs.
#'
#' @export
format_date_range_by_cal_id <- function(calendar, cal_ids, abbr = TRUE,
                                        days = FALSE, abbr_wday = NULL) {
  dates <- calendar %>%
    dplyr::filter(.data$cal_id %in% na.omit(cal_ids)) %>%
    dplyr::pull("date") %>%
    sanitize_date_range()

  if (days) {
    if (is.null(abbr_wday))
      abbr_wday = abbr
    format_day_date_range(dates, abbr_month = abbr, abbr_wday = abbr_wday)
  } else {
    format_date_range(dates, abbr)
  }
}

#' @describeIn lookup_and_format_date_range Look up dates by class number.
#'
#' @param nums A list or vector of class numbers.
#'
#' @export
format_date_range_by_class_num <- function(calendar, nums, abbr = TRUE,
                                           days = FALSE, abbr_wday = NULL) {
  col <- c()
  dates <- calendar %>%
    dplyr::filter(.data$class_num %in% na.omit(nums)) %>%
    dplyr::pull("date") %>%
    sanitize_date_range()
  if (days) {
    if (is.null(abbr_wday))
      abbr_wday = abbr
    format_day_date_range(dates, abbr_month = abbr, abbr_wday = abbr_wday)
  } else {
    format_date_range(dates, abbr)
  }
}


#' @describeIn lookup_and_format_date_range Look up dates by key
#'
#' @param keys A list or vector of key indices to look up.
#' @param type Which type of key is this ("class", "reading", "lab",
#'   "homework", "exam", "holiday", "event", "due date", or "raw")
#'
#' @details If the key type is `"raw"`, then the fully formatted key should
#'   be used (e.g., "CLS_INTRO" for a class key). Otherwise, the prefix will
#'   be added automatically based on the type, so `key = "INTRO", type = "lab"`
#'   will become `LAB_INTRO`.
#'
#' @export
format_date_range_by_key <- function(calendar, keys,
                                     type = c("class", "reading", "lab",
                                              "homework", "exam", "holiday",
                                              "event", "due date", "raw"),
                                     abbr = TRUE,
                                     days = FALSE, abbr_wday = NULL) {
  type <- match.arg(type)
  if (type == "reading") type <- "class"
  if (type != "raw") {
    keys <- add_key_prefix(keys, type)

  }

  dates <- calendar %>%
    dplyr::filter(.data$cal_key %in% keys) %>%
    dplyr::pull("date") %>%
    sanitize_date_range()
  if (days) {
    if (is.null(abbr_wday))
      abbr_wday = abbr
    format_day_date_range(dates, abbr_month = abbr, abbr_wday = abbr_wday)
  } else {
    format_date_range(dates, abbr)
  }
}



#' @describeIn lookup_and_format_date_range Look up dates by event id.
#'
#' @param event_ids A list or vector of event ids.
#'
#' @export
format_date_range_by_event_id <- function(calendar, event_ids, abbr = TRUE,
                                          days = FALSE, abbr_wday = NULL) {
  dates <- calendar %>%
    dplyr::filter(.data$event_id %in% event_ids) %>%
    dplyr::pull("date") %>%
    sanitize_date_range()
  if (days) {
    if (is.null(abbr_wday))
      abbr_wday = abbr
    format_day_date_range(dates, abbr_month = abbr, abbr_wday = abbr_wday)
  } else {
    format_date_range(dates, abbr)
  }
}


#' Format a Range of Pages
#'
#' Format a range of pages, accounting for both consecutive and non-consecutive
#'   pages.
#'
#' Figures out whether there is one page or more than one. Add the appropriate
#'   prefix ("p." or "pp.").
#'
#' @param pages A string describing the pages.
#'
#' @return Formatted page range.
#' @examples
#' format_page_range("99")
#' format_page_range("72 and 103")
#' format_page_range("50--75")
#' @export
format_page_range <- function(pages) {
  str <- stringr::str_trim(pages) %>% stringr::str_replace_all("^p+\\. *", "")
  multiple <- stringr::str_detect(pages, "-+|,|;| and ")
  stringr::str_c(ifelse(multiple, "pp. ", "p. "), pages)
}


#' Add a Period If Necessary
#'
#' Add a period at the end of a sentence if necessary.
#'
#' Trim any whitespace at the end, and if the line does not end with
#' punctuation, add a period.
#'
#' @param str A string, ending in a sentence.
#'
#' @return A string ending in punctuation.
#' @examples
#' add_period("This needs a period")
#' add_period("This does not need another period.")
#' add_period("Fix spaces at the end   ")
#' add_period("Shouldn't I omit a period if the sentence already ends with other punctuation?  ")
#'
#' @export
add_period <- function(str) {
  stringr::str_trim(str, "right") %>%
    stringr::str_replace("([^.?!])$", "\\1.")
}

item_format <- function(str, item_text, pad_len) {
  lines <- stringr::str_split(str, "\n") %>% purrr::simplify()
  pad_text <- stringr::str_dup(" ", stringr::str_length(item_text))
  left_pad <- c(item_text, rep(pad_text, length(lines) - 1))
  output <- stringr::str_c(stringr::str_dup(" ", pad_len),
                           left_pad, " ", lines) %>%
    stringr::str_trim("right") %>% stringr::str_c(collapse = "\n") %>%
    stringr::str_trim("right")
  if (stringr::str_detect(output, "\n\n")) {
    output <- stringr::str_c(output, "\n")
  }
  output
}


add_level <- function(pad_len = 0,
                      list_type = c("itemize", "enumerate", "definition"),
                      enum_type = "#.") {
  list_type = match.arg(list_type)
  if (list_type == "itemize") {
    pad_len <- pad_len + 2
  } else if (list_type == "enumerate") {
    pad_len <- pad_len + stringr::str_length(enum_type) + 1
  } else if (list_type == "definition") {
    pad_len <- pad_len + 4
  } else {
    stop("Illegal list type: ", list_type)
  }
  pad_len
}

itemize <- function(text, pad_len = 0) {
  purrr::map_chr(text, ~item_format(.x, "*", pad_len)) %>%
    stringr::str_c(collapse = "\n")
}


enumerate <- function(text, pad_len = 0, enum_type = "#.") {
  purrr::map_chr(text, ~item_format(.x, enum_type, pad_len)) %>%
    stringr::str_c(collapse = "\n")
}

