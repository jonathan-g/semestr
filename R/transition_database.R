transition_env <- new.env()

#
# Use as global variables, but encapsulate in an
# environment.
#
with( transition_env, {
  type2idx <- c(class = "class", lab = "lab", homework = "homework",
                "due date" = "due_date", exam = "exam", holiday = "holiday",
                event = "event")
  type2col <- c(class = "class", lab = "lab", homework = "hw",
                "due date" = "due", exam = "exam", holiday = "holiday",
                event = "evt")
  col2type <- purrr::set_names(names(type2col), type2col)
  idx2type <- purrr::set_names(names(type2idx), type2idx)
  # Prefixes are prefixed in front of topic keys in thelow
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

pull_env <- function(env) {
  for (n in ls(env)) {
    assign(n, get(n, envir = env), envir = parent.frame())
  }
}

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
#'   dplyr::selected. This can either be quoted or unquoted (i.e., using non-standard
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
do_index <- function(df, type, key_col = NULL) {
  pull_env(transition_env)

  assertthat::assert_that(! all(is.null(key_col), is.null(type)),
              msg = "Either type or key_col must be specified in do_index.")

  if (is.null(key_col)) {
    assertthat::assert_that(type %in% col2type,
                msg = stringr::str_c('Bad type name "', type, '" in do_index.'))
    key_col <- stringr::str_c(type2col[type], "_key")
  }
  qcol <- rlang::enquo(key_col)
  # message("enquosed")
  # print(qcol)
  if (rlang::is_character( rlang::quo_get_expr(qcol) )) {
    # message("character")
    key <- key_col
    key_col <- rlang::ensym(key_col)
    qcol <- rlang::enquo(key_col)
  } else {
    # message("not character")
    key <- as.character(rlang::quo_get_expr(qcol))
  }
  key <- stringr::str_replace(key, "_key$", "")
  # message("key = ", key)
  if (is.null(type)) {
    assertthat::assert_that(key %in% type2col)
    type <- col2type[key]
  }

  num_col_name <- stringr::str_c(key, "_num")
  num_col <- rlang::ensym(num_col_name)
  qnum <- rlang::enquo(num_col)
  if (as.character(rlang::quo_get_expr(qcol)) %in% names(df)) {
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
    df <- tibble::tibble(cal_id = integer(0), date = character(0),
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
reindex <- function(df, type, key_col = NULL, idx_col = NULL) {
  pull_env(transition_env)

  assertthat::assert_that(! all(is.null(type), is.null(key_col)),
              msg = "Either type or key_col must be specified in reindex.")

  if (is.null(key_col)) {
    assertthat::assert_that(type %in% col2type,
                msg = stringr::str_c('Bad type name "', type, '" in reindex.'))

    key_col <- stringr::str_c(type2col[type], "_key")
  }

  qkcol <- rlang::enquo(key_col)
  if (is_character(rlang::quo_get_expr(qkcol))) {
    key = key_col
    key_col <- rlang::ensym(key_col)
    qkcol <- rlang::enquo(key_col)
  } else {
    key = as.character(rlang::quo_get_expr(qkcol))
  }
  key_base <- key %>% stringr::str_replace("_key$", "")

  if (is.null(idx_col)) {
    idx_col <- stringr::str_c(key_base, "_num")
  }
  qicol <- rlang::enquo(idx_col)

  if (is_character(rlang::quo_get_expr(qicol))) {
    idx_col <- rlang::ensym(idx_col)
    qicol <- rlang::enquo(idx_col)
  }

  if (as.character(rlang::quo_get_expr(qicol)) %in% names(df)) {
    dfi <- df %>% dplyr::arrange(!!qicol) %>%
      dplyr::select(!!qkcol) %>% dplyr::distinct() %>%
      dplyr::mutate(!!qicol := seq_along(!!qkcol))
    dfx <- df %>% dplyr::select(-!!qicol) %>%
      dplyr::left_join(dfi, by = key)
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
#'   dplyr::selected. This can either be quoted or unquoted (i.e., using non-standard
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
cal_prepare <- function(df, type, key_col = NULL) {
  pull_env(transition_env)

  assertthat::assert_that(! all(is.null(type), is.null(key_col)),
              msg = "Either type or key_col must be specified in cal_prepare")

  if (is.null(key_col)) {
    assertthat::assert_that(type %in% col2type,
                msg = stringr::str_c('Bad type name: "', type, '" in cal_prepare'))
    key_col <- stringr::str_c(type2col[type], "_key")
  }
  qcol <- rlang::enquo(key_col)
  # message("enquosed")
  # print(qcol)
  if (is_character(rlang::quo_get_expr(qcol))) {
    # message("character")
    key <- key_col
    key_col <- rlang::ensym(key_col)
    qcol <- rlang::enquo(key_col)
  } else {
    # message("not character")
    key <- as.character(rlang::quo_get_expr(qcol))
  }
  key <- stringr::str_replace(key, "_key$", "")
  # message("key = ", key)
  if (is.null(type)) {
    assertthat::assert_that(key %in% type2col)
    type <- col2type[key]
  }
  prefix <- prefixes[type]

  if (as.character(rlang::quo_get_expr(qcol)) %in% names(df)) {
    df <- df %>%
      dplyr::mutate(topic_key = stringr::str_c(prefix, !!qcol, sep = "_")) %>%
      dplyr::select(-!!qcol)
  }
  invisible(df)
}


build_master_calendar <- function(master_db_file) {
  pull_env(transition_env)

  master_db <- DBI::dbConnect(RSQLite::SQLite(), master_db_file)

  cal <- dplyr::tbl(master_db, "master_calendar") %>% dplyr::collect()

  DBI::dbDisconnect(master_db)

  base_classes <- cal %>% do_index("class") %>% reindex("class") %>%
    dplyr::mutate(week = class_num %/% 3 + 1)
  labs <- cal %>% do_index("lab") %>% reindex("lab")
  exams <- cal %>% do_index("exam") %>% reindex("exam")
  homework <- cal %>% do_index("homework") %>% reindex("homework")
  due_dates <- cal %>% do_index("due date") %>% reindex("due date")
  holidays <- cal %>% do_index("holiday") %>% reindex("holiday")
  events <- cal %>% do_index("event") %>% reindex("event")

  topics <- cal %>% dplyr::select(topic_key = class_key, topic) %>%
    dplyr::filter(! is.na(topic_key), ! is.na(topic)) %>%
    add_key_prefix("class")

  calendar <- dplyr::bind_rows(
    cal_prepare(base_classes, "class"),
    cal_prepare(labs, "lab"),
    cal_prepare(exams, "exam"),
    cal_prepare(homework, "homework"),
    cal_prepare(due_dates, "due date"),
    cal_prepare(holidays, "holiday"),
    cal_prepare(events, "event")
  ) %>% dplyr::arrange(date, cal_id) %>%
    dplyr::select(cal_id, date, class_num, week, topic_key, cal_type,
                  cancelled, make_up, tidyselect::everything())

  classes <- cal %>% do_index("class") %>% reindex("class") %>%
    dplyr::mutate(week = class_num %/% 3 + 1)

  labs <- cal %>% do_index("lab") %>% reindex("lab")
  exams <- cal %>% do_index("exam") %>% reindex("exam")
  homework <- cal %>% do_index("homework") %>% reindex("homework")
  due_dates <- cal %>% do_index("due date") %>% reindex("due date")
  holidays <- cal %>% do_index("holiday") %>% reindex("holiday")
  events <- cal %>% do_index("event") %>% reindex("event")

  calendar <- dplyr::bind_rows(
    cal_prepare(classes, "class"),
    cal_prepare(labs, "lab"),
    cal_prepare(exams, "exam"),
    cal_prepare(homework, "homework"),
    cal_prepare(due_dates, "due date"),
    cal_prepare(holidays, "holiday"),
    cal_prepare(events, "event")
  ) %>% dplyr::arrange(date, cal_id) %>%
    dplyr::select(cal_id, date, class_num, week, topic_key, cal_type,
                  cancelled, make_up, tidyselect::everything())

  calendar <- calendar %>% left_join(topics, by = "topic_key")

  invisible(calendar)
}

strip_key_prefix <- function(df, type) {
  pull_env(transition_env)

  target <- stringr::str_c("^", prefixes[type], "_")
  df <- df %>% dplyr::mutate(topic_key = stringr::str_replace(topic_key, target, ""))
  invisible(df)
}

add_key_prefix <- function(df, type) {
  pull_env(transition_env)

  df <- df %>% dplyr::mutate(topic_key =
                             stringr::str_c(prefixes[type], topic_key, sep = "_"))
  invisible(df)
}

extract_links <- function(df, type, num_col = NULL) {
  pull_env(transition_env)

  if (is.null(num_col)) {
    assertthat::assert_that(type %in% col2type,
                msg = stringr::str_c('Bad type name "', type, '" in extract_links.'))
    num_col <- stringr::str_c(type2col[type], "_num")
  }
  num_col <- rlang::ensym(num_col)
  df <- df %>% dplyr::filter(cal_type == type) %>%
    dplyr::select(cal_id, topic_key, !!num_col) %>%
    strip_key_prefix(type)
  invisible(df)
}

extract_topic_keys <- function(cal, type) {
  pull_env(transition_env)

  cal %>% dplyr::filter(cal_type == type) %>% strip_key_prefix(type) %$%
    topic_key
}

check_link_keys <- function(cal, links, type) {
  link_keys <- links$topic_key
  cal_keys <- extract_topic_keys(cal, type)
  setdiff(link_keys, cal_keys)
}

build_database <- function(dest_db_file, master_db_file, base_db_file) {
  pull_env(transition_env)

  master_calendar <- build_master_calendar(master_db_file)

  calendar <- master_calendar %>%
    dplyr::select(cal_id, date, class_num, week, topic_key, cal_type, cancelled,
           make_up) %>% dplyr::arrange(date, cal_id, cal_type)

  class_topics <- master_calendar %>% dplyr::filter(cal_type == "class") %>%
    dplyr::select(topic_key, topic) %>%
    strip_key_prefix("class")

  class_links   <- extract_links(master_calendar, "class"   )
  lab_links     <- extract_links(master_calendar, "lab"     )
  hw_links      <- extract_links(master_calendar, "homework")
  exam_links    <- extract_links(master_calendar, "exam"    )
  due_links     <- extract_links(master_calendar, "due date")
  holiday_links <- extract_links(master_calendar, "holiday" )
  event_links   <- extract_links(master_calendar, "event"   )

  base_db <- DBI::dbConnect(RSQLite::SQLite(), base_db_file)

  exams <- dplyr::tbl(base_db, "exams") %>%
    dplyr::select(-exam_id) %>% dplyr::collect()
  events <- dplyr::tbl(base_db, "events") %>%
    dplyr::select(-event_id) %>% dplyr::collect()
  holidays <- dplyr::tbl(base_db, "holidays") %>%
    dplyr::select(-holiday_id) %>% dplyr::collect()
  notices <- dplyr::tbl(base_db, "notices") %>%
    dplyr::select(-notice_id) %>% dplyr::collect()

  reading_items <- dplyr::tbl(base_db, "reading_items") %>% dplyr::collect()
  reading_sources <- dplyr::tbl(base_db, "reading_sources") %>% dplyr::collect()

  hw_assignments <- dplyr::tbl(base_db, "homework_assignments") %>%
    dplyr::collect()
  hw_items <- dplyr::tbl(base_db, "homework_items") %>% dplyr::collect()
  hw_topics <- dplyr::tbl(base_db, "homework_topics") %>% dplyr::collect()
  hw_solutions <- dplyr::tbl(base_db, "homework_solutions") %>% dplyr::collect()

  lab_assignments <- dplyr::tbl(base_db, "lab_assignments") %>% dplyr::collect()
  lab_items <- dplyr::tbl(base_db, "lab_items") %>% dplyr::collect()
  lab_solutions <- dplyr::tbl(base_db, "lab_solutions") %>% dplyr::collect()

  due_dates <- tibble(due_key = character(0), due_title = character(0),
                      due_desc = character(0))

  text_codes <- dplyr::tbl(base_db, "text_codes") %>% dplyr::collect()

  DBI::dbDisconnect(base_db)

  db_dest <- DBI::dbConnect(RSQLite::SQLite(), dest_db_file)

  #
  # Calendar
  #
  copy_to(db_dest, calendar, "calendar", overwrite = TRUE, temporary = FALSE)

  #
  # Links
  #
  copy_to(db_dest, class_links, "rd_links", overwrite = TRUE, temporary = FALSE)
  copy_to(db_dest, lab_links, "lab_links", overwrite = TRUE, temporary = FALSE)
  copy_to(db_dest, hw_links, "hw_links", overwrite = TRUE, temporary = FALSE)
  copy_to(db_dest, due_links, "due_links", overwrite = TRUE, temporary = FALSE)
  copy_to(db_dest, exam_links, "exam_links", overwrite = TRUE,
          temporary = FALSE)
  copy_to(db_dest, holiday_links, "holiday_links", overwrite = TRUE,
          temporary = FALSE)
  copy_to(db_dest, event_links, "event_links", overwrite = TRUE,
          temporary = FALSE)

  #
  # Reading assignments
  #
  copy_to(db_dest, class_topics, "class_topics", overwrite = TRUE, temporary = FALSE)
  copy_to(db_dest, reading_items, "rd_items", overwrite = TRUE,
          temporary = FALSE)
  copy_to(db_dest, reading_sources, "rd_src", overwrite = TRUE,
          temporary = FALSE)

  #
  # Homework assignments
  #
  copy_to(db_dest, hw_assignments, "hw_asgt", overwrite = TRUE,
          temporary = FALSE)
  copy_to(db_dest, hw_items, "hw_items", overwrite = TRUE, temporary = FALSE)
  copy_to(db_dest, hw_solutions, "hw_sol", overwrite = TRUE, temporary = FALSE)

  #
  # Lab Assignments
  #
  copy_to(db_dest, lab_assignments, "lab_asgt", overwrite = TRUE,
          temporary = FALSE)
  copy_to(db_dest, lab_items, "lab_items", overwrite = TRUE, temporary = FALSE)
  copy_to(db_dest, lab_solutions, "lab_sol", overwrite = TRUE,
          temporary = FALSE)

  #
  # Due dates
  #
  copy_to(db_dest, due_dates, "due_dates", overwrite = TRUE,
          temporary = FALSE)

  #
  # Events
  #
  copy_to(db_dest, events, "events", overwrite = TRUE,
          temporary = FALSE)
  copy_to(db_dest, notices, "notices", overwrite = TRUE,
          temporary = FALSE)
  copy_to(db_dest, holidays, "holidays", overwrite = TRUE,
          temporary = FALSE)



  DBI::dbDisconnect(db_dest)

}


