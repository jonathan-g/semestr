transition.env <- new.env()

with( transition.env, {
  prefixes <- c(class = "CLS", lab = "LAB", homework = "HW", due_date = "DUE",
                exam = "EXAM", holiday = "VAc", event = "EVT")
  bases <- c(class = 1000, lab = 2000, homework = 3000, due_date = 4000,
             exam = 5000,holiday = 6000, event = 7000)
  base_mods <- c(cancelled = 100, make_up = 200), envir = semestr.env)
})

build_base_calendar <- function(master_cal) {

}

do_index <- function(df, key_col, type = NULL) {
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
  num_col_name <- stringr::str_c(key, "_num")
  num_col <- ensym(num_col_name)
  qnum <- enquo(num_col)
  df <- df %>% dplyr::select(cal_id, date, !!qcol, cancelled, make_up) %>%
    dplyr::arrange(date) %>%
    dplyr::filter(!is.na(!!qcol)) %>%
    dplyr::mutate(cal_type = type,
           !!num_col := seq_along(date),
           cal_id = bases[type] + !!num_col +
             base_mods['cancelled'] * cancelled +
             base_mods['make_up'] * make_up
    )
  invisible(df)
}

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
  dfi <- df %>% dplyr::arrange(!!qicol) %>%
    dplyr::select(!!qkcol) %>% distinct() %>%
    dplyr::mutate(!!qicol := seq_along(!!qkcol))
  dfx <- df %>% dplyr::select(-!!qicol) %>%
    left_join(dfi, by = key)
  invisible(dfx)
}

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

  df <- df %>% dplyr::mutate(topic_key = stringr::str_c(prefix, !!qcol, sep = "_")) %>%
    dplyr::select(-!!qcol)
  invisible(df)
}


transition_database <- function(dest_file = "semester.sqlite3",
                                master_file = "alt_semester.sqlite3",
                                base_file = "new_semester.sqlite3") {
  base_dir <- rprojroot::find_root(rprojroot::has_file("DESCRIPTION"))
  planning_dir <- file.path(base_dir, "planning")

  attach(transition.env)

  master_db <- DBI::dbConnect(RSQLite::SQLite(), file.path(planning_dir, master_file))

  cal <- dplyr::tbl(master_db, "calendar") %>% dplyr::collect()

  base_classes <- cal %>% do_index(class_key) %>% reindex(class_key) %>%
    dplyr::mutate(week = class_num %/% 3 + 1)

  labs <- cal %>% do_index(lab_key) %>% reindex(lab_key)

  exams <- cal %>% do_index(exam_key) %>% reindex(exam_key)

  #
  # homework <- cal %>% do_index(hw_key, "homework") %>% reindex(event_key)
  #
  # due_dates <- cal %>% do_index(due_key, "due_date") %>% reindex(due_key)
  #

  holidays <- cal %>% do_index(holiday_key) %>% reindex(holiday_key)

  events <- cal %>% do_index(event_key) %>% reindex(event_key)

  calendar <- bind_rows(
    cal_prepare(classes, class_key),
    cal_prepare(labs, lab_key),
    cal_prepare(exams, exam_key),
    # cal_prepare(homework, hw_key, "homework"),
    # cal_prepare(due_dates, due_key, "due_date"),
    cal_prepare(holidays, holiday_key),
    cal_prepare(events, event_key)
  ) %>% dplyr::arrange(date, cal_id) %>%
    dplyr::select(cal_id, date, class_num, week, topic_key, cal_type, cancelled,
           make_up, everything())

  classes <- cal %>% do_index(class_key) %>% reindex(class_key) %>%
    dplyr::mutate(week = class_num %/% 3 + 1)

  labs <- cal %>% do_index(lab_key) %>% reindex(lab_key)

  exams <- cal %>% do_index(exam_key) %>% reindex(exam_key)

  #
  # homework <- cal %>% do_index(hw_key, "homework") %>% reindex(event_key)
  #
  # due_dates <- cal %>% do_index(due_key, "due_date") %>% reindex(due_key)
  #

  holidays <- cal %>% do_index(holiday_key) %>% reindex(holiday_key)

  events <- cal %>% do_index(event_key) %>% reindex(event_key)

  calendar <- bind_rows(
    cal_prepare(classes, class_key),
    cal_prepare(labs, lab_key),
    cal_prepare(exams, exam_key),
    # cal_prepare(homework, hw_key, "homework"),
    # cal_prepare(due_dates, due_key, "due_date"),
    cal_prepare(holidays, holiday_key),
    cal_prepare(events, event_key)
  ) %>% dplyr::arrange(date, cal_id) %>%
    dplyr::select(cal_id, date, class_num, week, topic_key, cal_type, cancelled,
           make_up, everything())


}


