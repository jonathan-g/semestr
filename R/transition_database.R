transition.env <- new.env()

#
# Use as global variables, but encapsulate in an
# environment.
#
with( transition.env, {
  # Prefixes are prefixed in front of topic keys in the
  # calendar to identify what kind of item they refer to.
  prefixes <- c(class = "CLS", lab = "LAB", homework = "HW", due_date = "DUE",
                exam = "EXAM", holiday = "VAC", event = "EVT")
  # The 1000s place in the calendar id identifies the kind of item
  # that id refers to.
  bases <- c(class = 1000, lab = 2000, homework = 3000, due_date = 4000,
             exam = 5000, holiday = 6000, event = 7000)
  # The 100s place in a calendar item is used to indicate
  # re-scheduling from the regular class time.
  base_mods <- c(cancelled = 100, make_up = 200)
})

#' Extract and index calendar entries by type.
#'
#' Extract calendar entries of a given type. Add an index indicating the
#' sequence of entries with that type, and add a column indicating what
#' the type is.
#'
#' The type is based on the `key_col` argument: Entries with a key in that
#' column are chosen, and the numerical index column is created using a name
#' derived by substituting "`_num`" ror "`_key`" in the column name (e.g.,
#' "`class_key`" becomes "`class_num`"). The index represents the sequence of
#' events of the chosen type, ordered by date.
#'
#' @param df A dataframe holding the calendar. Columns should include:
#' \describe{
#'   \item{date}{The date of the calendar item}
#'   \item{`key_col`}{A character string with the key for the calendar entry.
#'     This column's name should match the `key_col` argument.}
#'   \item{cancelled}{An integer indicating whether the entry has been
#'     cancelled for the date of this entry.}
#'   \item{make_up}{An integer indicating whether the entry represents a
#'     re-scheduled make-up event}
#' }
#' @param key_col The name of the column with the keys for the entries to be
#'   selected. This can either be quoted or unquoted (i.e., using non-standard
#'   tidy evaluation). If the `key_col` argument is missing and `type` is not
#'   `NULL`, then a default `key_col` will be constructed by appending "`_key`"
#'   to `type` (e.g., "`class`" becomes "`class_key`").
#' @param type The type of event. This should be one of `class`, `lab`,
#'   `homework`, `due date`, `exam`, `holiday`, or `event`.
#'   If `type` is `NULL`, the value is constructed by removing "`_key`" from
#'   the end of the `key_col` argument (e.g., "`class_key`" becomes "`class`").
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{cal_id}{An integer whose thousands place indicates the kind of
#'     entry, hundreds place indicates re-scheduling (cancellation or make-up),
#'     and last two digits indicate the order of the entry.}
#'   \item{date}{The date of the entry, as an ISO format string.}
#'   \item{cancelled}{An integer indicating whether the entry has been
#'     cancelled.}
#'   \item{make_up}{An integer indicating whether the entry represents a
#'     re-scheduled make-up session.}
#'   \item{`_key`}{A column with a text key for that entry. The name of this
#'     column is the `key_col` argument.}
#'   \item{`_num`}{A column with an integer index representing the order of the
#'     entry in the sequence of this kind of entry (class, lab, etc.). The
#'     name of this column is constructed by replacing "`_key`" with "`_num`"
#'     in the `key_col` argument (e.g., "`class_key`" becomes "`class_num`").}
#'   \item{cal_type}{A string indicating the type of entry (class, lab, etc.)}
#' }
#'
do_index <- function(df, key_col, type = NULL) {
  if (missing(key_col)) {
    key_col <- stringr::str_c(type, "_key")
  }
  qcol <- enquo(key_col)
  # message("enquosed")
  print(qcol)
  if (is_character(quo_get_expr(qcol))) {
    # message("character")
    key <- key_col
    key_col <- ensym(key_col)
    qcol <- enquo(key_col)
  } else {
    # message("not character")
    key <- as.character(quo_get_expr(qcol))
  }
  key <- stringr::str_replace(key, "_key$", "")
  # message("key = ", key)
  if (is.null(type)) {
    type <- key
  } else {
    if (! str_replace_all(type, " +", "_") %in% names(prefixes)) {
      stop('Bad type name "', type, '".')
    }
  }
  num_col_name <- stringr::str_c(key, "_num")
  num_col <- ensym(num_col_name)
  qnum <- enquo(num_col)
  if (as.character(quo_get_expr(qcol)) %in% names(df)) {
    df <- df %>% dplyr::select(date, !!qcol, cancelled, make_up) %>%
      dplyr::arrange(date) %>%
      dplyr::filter(!is.na(!!qcol)) %>%
      dplyr::mutate(cal_type = type,
                    !!num_col := seq_along(date),
                    cal_id = bases[type] + !!num_col +
                      base_mods['cancelled'] * cancelled +
                      base_mods['make_up'] * make_up
      )
  } else {
    df <- tibble(cal_id = integer(0), date = character(0),
                 !!qcol := character(0),
                 cancelled = integer(0), make_up = integer(0),
                 cal_type = character(0),
                 !!num_col := integer(0))
  }
  invisible(df)
}

#' Re-index calendar entries of a single type.
#'
#' Take a calendar with entries of a given type and re-index them so that
#' repeated entries with the same text key get the same numerical index.
#' For instance, when there is a week-long holiday, such as Thanksgiving or
#' Spring Break, there may be two or three entries with the same key, but on
#' different dates. This function makes sure that they are all assigned the
#' same numerical index.
#'
#' @param df A dataframe holding calendar events of a given type.
#'   Columns should include:
#' \describe{
#'   \item{cal_id}{An integer whose thousands place indicates the kind of
#'     entry, hundreds place indicates re-scheduling (cancellation or make-up),
#'     and last two digits indicate the order of the entry.}
#'   \item{date}{The date of the entry, as an ISO format string.}
#'   \item{cancelled}{An integer indicating whether the entry has been
#'     cancelled.}
#'   \item{make_up}{An integer indicating whether the entry represents a
#'     re-scheduled make-up session.}
#'   \item{`key_col`}{A column with a text key for that entry.}
#'   \item{`idx_col`}{A column with an integer index representing the sequence
#'     of this kind of entry (class, lab, etc.)}
#'   \item{cal_type}{A string indicating the type of entry (class, lab, etc.)}
#' }
#' @param key_col The name of the column with the keys for the entries.
#'   This can either be quoted or unquoted (i.e., using non-standard tidy
#'   evaluation).
#' @param idx_col The name of the column with the integer index for entries
#'   `homework`, `due date`, `exam`, `holiday`, or `event`.
#'   If `idx_col` is `NULL`, the value is constructed by replacing "`_key`" with
#'   "`_num`" in the `key_col` argument (e.g., "`class_key`" becomes
#'   "`class_num`").
#'
#' @return A tibble with the same columns as the input `df`, but with the
#'   numerical index in `idx_col` adjusted so multiple entries with the same
#'   key in `key_col` have the same numerical index in `idx_col`.
#'
reindex <- function(df, key_col, idx_col = NULL) {
  qkcol <- enquo(key_col)
  if (is_character(quo_get_expr(qkcol))) {
    key = key_col
    key_col <- ensym(key_col)
    qkcol <- enquo(key_col)
  } else {
    key = as.character(quo_get_expr(qkcol))
  }
  key_base <- key %>% stringr::str_replace("_key$", "")

  if (is.null(idx_col)) {
    idx_col <- stringr::str_c(key_base, "_num")
  }
  qicol <- enquo(idx_col)

  if (is_character(quo_get_expr(qicol))) {
    idx_col <- ensym(idx_col)
    qicol <- enquo(idx_col)
  }

  if (as.character(quo_get_expr(qicol)) %in% names(df)) {
    dfi <- df %>% dplyr::arrange(!!qicol) %>%
      dplyr::select(!!qkcol) %>% distinct() %>%
      dplyr::mutate(!!qicol := seq_along(!!qkcol))
    dfx <- df %>% dplyr::select(-!!qicol) %>%
      left_join(dfi, by = key)
  } else {
    dfx <- df
  }
  invisible(dfx)
}

#' Convert an event-specific calendar tibble to a general master calendar.
#'
#' Convert the columns of a calendar specific to a certain kind of entry to
#' generic columns suitable for a master calendar containing multiple kinds of
#' entries.
#'
#' @param df A dataframe holding the calendar. Columns should include:
#' \describe{
#'   \item{date}{The date of the calendar item}
#'   \item{`key_col`}{A character string with the key for the calendar entry.
#'     This column's name should match the `key_col` argument.}
#'   \item{cancelled}{An integer indicating whether the entry has been
#'     cancelled for the date of this entry.}
#'   \item{make_up}{An integer indicating whether the entry represents a
#'     re-scheduled make-up event}
#' }
#' @param key_col The name of the column with the keys for the entries to be
#'   selected. This can either be quoted or unquoted (i.e., using non-standard
#'   tidy evaluation). If the `key_col` argument is missing and `type` is not
#'   `NULL`, then a default `key_col` will be constructed by appending "`_key`"
#'   to `type` (e.g., "`class`" becomes "`class_key`").
#' @param type The type of event. This should be one of `class`, `lab`,
#'   `homework`, `due date`, `exam`, `holiday`, or `event`.
#'   If `type` is `NULL`, the value is constructed by removing "`_key`" from
#'   the end of the `key_col` argument (e.g., "`class_key`" becomes "`class`").
#'
#' @return A tibble with columns:
#' \describe{
#'   \item{cal_id}{An integer whose thousands place indicates the kind of
#'     entry, hundreds place indicates re-scheduling (cancellation or make-up),
#'     and last two digits indicate the order of the entry.}
#'   \item{date}{The date of the entry, as an ISO format string.}
#'   \item{cancelled}{An integer indicating whether the entry has been
#'     cancelled.}
#'   \item{make_up}{An integer indicating whether the entry represents a
#'     re-scheduled make-up session.}
#'   \item{topic_key}{A column with a text key for that entry. }
#'   \item{cal_type}{A string indicating the type of entry (class, lab, etc.)}
#'   \item{...}{Other entries, such as a numerical index for events of a
#'     given type. In general, we expect (but do not require) that there is
#'     a numerical index column corresponding to the `key_col` argument
#'     (e.g., if `key_col` = "`lab_key`", then there should be a "`lab_num`"
#'     column corresponding to the sequence of laboratory entries).}
#' }
#'
cal_prepare <- function(df, key_col, type = NULL) {
  if (missing(key_col)) {
    key_col <- stringr::str_c(type, "_key")
  }
  qcol <- enquo(key_col)
  # message("enquosed")
  # print(qcol)
  if (is_character(quo_get_expr(qcol))) {
    # message("character")
    key <- key_col
    key_col <- ensym(key_col)
    qcol <- enquo(key_col)
  } else {
    # message("not character")
    key <- as.character(quo_get_expr(qcol))
  }
  key <- stringr::str_replace(key, "_key$", "")
  # message("key = ", key)
  if (is.null(type)) {
    type <- key
  }
  prefix <- prefixes[type]

  if (as.character(quo_get_expr(qcol)) %in% names(df)) {
    df <- df %>%
      dplyr::mutate(topic_key = stringr::str_c(prefix, !!qcol, sep = "_")) %>%
      dplyr::select(-!!qcol)
  }
  invisible(df)
}


transition_database <- function(dest_file = "semester.sqlite3",
                                master_file = "alt_semester.sqlite3",
                                base_file = "new_semester.sqlite3") {
  base_dir <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
  planning_dir <- file.path(base_dir, "planning")

  attach(transition.env)

  master_db <- DBI::dbConnect(RSQLite::SQLite(),
                              file.path(planning_dir, master_file))

  cal <- dplyr::tbl(master_db, "master_calendar") %>% dplyr::collect()

  base_classes <- cal %>% do_index(class_key) %>% reindex(class_key) %>%
    dplyr::mutate(week = class_num %/% 3 + 1)
  labs <- cal %>% do_index(lab_key) %>% reindex(lab_key)
  exams <- cal %>% do_index(exam_key) %>% reindex(exam_key)
  homework <- cal %>% do_index(hw_key, "homework") %>% reindex(event_key)
  due_dates <- cal %>% do_index(due_key, "due_date") %>% reindex(due_key)
  holidays <- cal %>% do_index(holiday_key) %>% reindex(holiday_key)
  events <- cal %>% do_index(event_key) %>% reindex(event_key)

  calendar <- bind_rows(
    cal_prepare(base_classes, class_key),
    cal_prepare(labs, lab_key),
    cal_prepare(exams, exam_key),
    cal_prepare(homework, hw_key, "homework"),
    cal_prepare(due_dates, due_key, "due_date"),
    cal_prepare(holidays, holiday_key),
    cal_prepare(events, event_key)
  ) %>% dplyr::arrange(date, cal_id) %>%
    dplyr::select(cal_id, date, class_num, week, topic_key, cal_type,
                  cancelled, make_up, everything())

  classes <- cal %>% do_index(class_key) %>% reindex(class_key) %>%
    dplyr::mutate(week = class_num %/% 3 + 1)

  labs <- cal %>% do_index(lab_key) %>% reindex(lab_key)
  exams <- cal %>% do_index(exam_key) %>% reindex(exam_key)
  homework <- cal %>% do_index(hw_key, "homework") %>% reindex(event_key)
  due_dates <- cal %>% do_index(due_key, "due_date") %>% reindex(due_key)
  holidays <- cal %>% do_index(holiday_key) %>% reindex(holiday_key)
  events <- cal %>% do_index(event_key) %>% reindex(event_key)

  calendar <- bind_rows(
    cal_prepare(classes, class_key),
    cal_prepare(labs, lab_key),
    cal_prepare(exams, exam_key),
    cal_prepare(homework, hw_key, "homework"),
    cal_prepare(due_dates, due_key, "due_date"),
    cal_prepare(holidays, holiday_key),
    cal_prepare(events, event_key)
  ) %>% dplyr::arrange(date, cal_id) %>%
    dplyr::select(cal_id, date, class_num, week, topic_key, cal_type,
                  cancelled, make_up, everything())
}


