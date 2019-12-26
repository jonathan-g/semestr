#' Expose contents of an environment in the current environment
#'
#' Expose the contents of an environment in the current environment.
#' Similar to [attach], but it exposes the contents only in the
#' current environment and does not change the global search.
#'
#' @param env The environment to attach locally.
#'
pull_env <- function(env) {
  for (n in ls(env)) {
    assign(n, get(n, envir = env), envir = parent.frame())
  }
}

#' Determine the type of calendar entry from its calendar id.
#'
#' @param cal_id an integer calendar ID number.
#' @param metadata A list of metadata as returned from [load_semester_db].
#'
#' @return A string identifying the type of calendar entry. Current values
#'   are "class", "reading", "homework", "lab", "exam", "due date", "holiday",
#'   and "event".
#'
item_type <- function(cal_id, metadata) {
  base <- as.integer(cal_id) %>%
    divide_by_int(1000) %>% multiply_by(1000) %>% as.character()
  metadata$rev_base[base]
}

#' Determine the modification type of calendar entry from its calendar id.
#'
#' Modifications include cancelled and re-scheduled (make-up) classes.
#'
#' @param cal_id an integer calendar ID number.
#' @param metadata A list of metadata as returned from [load_semester_db].
#'
#' @return A string identifying the type of modification. Current values are
#'   "cancelled" and "make-up"
#'
item_mod <- function(cal_id, metadata) {
  base_mod <- as.integer(cal_id) %/% mod(1000) %>%
    divide_by_int(100) %>% multiply_by(100) %>% as.character()
  metadata$rev_mod[base_mod]
}

#' Strip prefixes off keys.
#'
#' Keys in the master calendar's `topic_key` column have prefixes according to
#' the type of calennder entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function strips those off.
#'
#' @param df The data frame to process.
#' @param metadata A list containing metadata for the differennt types.
#' @param type The type of calendar entry to strip (e.g., "`class`", "`lab`",
#' etc.)
#' @param col The column where the keys are located (by default "`topic_key`").
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso add_key_prefix
#'
strip_key_prefix <- function(df, metadata, type, col = "topic_key") {
  col <- ensym(col)
  col <- enquo(col)

  target <- stringr::str_c("^", metadata$prefixes[metadata$type2idx[type]], "_")
  df <- df %>% dplyr::mutate(!!col := stringr::str_replace(!!col, target, ""))

  invisible(df)
}

#' Add prefixes to keys.
#'
#' Keys in the master calendar's `topic_key` column have prefixes according to
#' the type of calennder entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function adds those prefixes.
#'
#' @param df The data frame to process.
#' @param metadata A list containing metadata for the differennt types.
#' @param type The type of prefix to add (e.g., "`class`", "`lab`",
#' etc.)
#' @param col The column where the keys are located (by default "`topic_key`").
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso strip_key_prefix
#'
add_key_prefix <- function(df, metadata, type, col = "topic_key") {
  col <- ensym(col)
  col <- enquo(col)

  df <- df %>% dplyr::mutate(!!col := stringr::str_c(prefixes[type], !!col,
                                                     sep = "_"))
  invisible(df)
}

#' Load schedule for semester from database
#'
#' Loads schedule for class meetings, reading and homework assignments,
#' lab sessions, exams, holidays, etc. from a database.
#'
#' @param db_file An SQLite database file.
#'
#' @return A list containing a tibbles with the calendar for the semester and
#'   tibbles with details on reading assignments, homework assignments,
#'   labs, exams, holidays, and other events.
#'
#'   THe list contains the following tibbles:
#'   `calendar`, `due_dates`, `due_links`, `rd_items`, `rd_src`, `rd_links`,
#'   `class_topics`, `hw_asgt`, `hw_items`, `hw_sol`, `hw_links`, `hw_topics`,
#'   `lab_asgt`, `lab_items`, `lab_sol`, `lab_links`, `exams`, `exam_links`,
#'   `holidays`, `holiday_links`, `events`, `event_links`, `notices`,
#'   `text_codes`
#'
#'   and a named list `metadata` containing named character vectors of
#'   metadata that are used to decode and manipulate calendar entries.
#'
load_semester_db <- function(db_file) {
  db <- DBI::dbConnect(RSQLite::SQLite(), db_file)

  md_1 <- dplyr::tbl(db, "metadata") %>% dplyr::collect()
  md_2 <- dplyr::tbl(db, "base_mods") %>% dplyr::collect()

  type2idx  <- purrr::set_names(md_1$idx, md_1$type)
  idx2type  <- purrr::set_names(md_1$type, md_1$idx)
  type2col  <- purrr::set_names(md_1$col, md_1$type)
  col2type  <- purrr::set_names(md_1$type, md_1$col)
  idx2col   <- purrr::set_names(md_1$col, md_1$idx)
  col2idx   <- purrr::set_names(md_1$idx, md_1$col)
  prefixes  <- purrr::set_names(md_1$prefix, md_1$type)
  bases     <- purrr::set_names(md_1$base, md_1$type)
  rev_base  <- purrr::set_names(md_1$type, as.character(md_1$base))

  mods <- purrr::set_names(md_2$mod, md_2$key)
  rev_mods <- purrr::set_names(md_2$key, as.character(md_2$mod))

  metadata <- list(type2idx = type2idx, type2col = type2col,
                   idx2type = idx2type, col2type = col2type,
                   idx2col  = idx2col,  col2idx  = col2idx,
                   prefixes = prefixes, bases = bases, rev_base = rev_base,
                   mods = mods, rev_mods = rev_mods)

  for (t in c("calendar", "due_dates", "events",
              "exams", "holidays", "notices",
              "due_links", "event_links", "exam_links", "holiday_links",
              "hw_links", "lab_links", "rd_links",
              "hw_asgt", "hw_items", "hw_sol", "hw_topics",
              "lab_asgt", "lab_items", "lab_sol", # "lab_topics",
              "rd_items", "rd_src", "class_topics",
              "text_codes")) {
    df <- dplyr::tbl(db, t) %>% dplyr::collect()
    assign(t, df)
  }

  DBI::dbDisconnect(db)

  due_dates <- due_links %>% dplyr::left_join(due_dates, by = "due_key")

  hw_asgt <- hw_asgt %>% dplyr::inner_join(hw_links, by = "hw_key") %>%
    dplyr::left_join(hw_topics, by = "hw_key") %>%
    dplyr::left_join(due_dates, by = c("hw_due_key" = "due_key")) %>%
    dplyr::mutate_at(dplyr::vars(hw_is_numbered),
                     ~as.logical(.) %>% tidyr::replace_na(FALSE))
  hw_items <- hw_items %>% dplyr::inner_join(hw_links, by = "hw_key") %>%
    dplyr::mutate_at(dplyr::vars(undergraduate_only, graduate_only,
                                 hw_break_before, hw_prologue, hw_epilogue),
                     ~as.logical(.) %>% tidyr::replace_na(FALSE))
  hw_sol <- hw_sol %>% dplyr::inner_join(hw_links, by = "hw_key") %>%
    dplyr::inner_join( dplyr::select(due_dates, hw_sol_pub_key = due_key,
                                     sol_pub_cal_id  = cal_id),
                       by = "hw_sol_pub_key")

  rd_items <- rd_items %>% dplyr::inner_join(rd_links, by = "rd_key") %>%
    dplyr::left_join(rd_src, by = "src_key") %>%
    dplyr::mutate_at(dplyr::vars(undergraduate_only, graduate_only, optional,
                                 rd_prologue, rd_epilogue, textbook, handout),
                     ~as.logical(.) %>% tidyr::replace_na(FALSE))

  lab_asgt <- lab_asgt %>% dplyr::inner_join(lab_links, by = "lab_key") %>%
    dplyr::left_join( dplyr::select(due_dates, report_due_key = due_key,
                                    report_cal_id = cal_id),
                      by = "report_due_key") %>%
    dplyr::left_join( dplyr::select(due_dates, presentation_key = due_key,
                                    pres_cal_id = cal_id),
                      by = "presentation_key")
  lab_items <- lab_items %>% dplyr::inner_join(lab_links, by = "lab_key")
  lab_sol <- lab_sol %>% dplyr::inner_join(lab_links, by = "lab_key") %>%
    dplyr::inner_join( dplyr::select(due_dates, lab_sol_pub_key = due_key,
                                     sol_pub_cal_id  = cal_id),
                       by = "lab_sol_pub_key")

  events <- events %>% dplyr::inner_join(event_links, by = "event_key")
  exams <- exams %>% dplyr::inner_join(exam_links, by = "exam_key")
  holidays <- holidays %>% dplyr::inner_join(holiday_links, by = "holiday_key")
  notices <- notices %>%
    dplyr::inner_join( dplyr::select(calendar, cal_id, topic_key),
                       by = "topic_key")

  semester <- list(
    calendar = calendar, due_dates = due_dates, due_links = due_links,
    rd_items = rd_items, rd_src = rd_src, rd_links = rd_links,
    class_topics = class_topics,
    hw_asgt = hw_asgt, hw_items = hw_items, hw_sol = hw_sol,
    hw_links = hw_links, hw_topics = hw_topics,
    lab_asgt = lab_asgt, lab_items = lab_items, lab_sol = lab_sol,
    lab_links = lab_links,
    exams = exams, exam_links = exam_links,
    holidays = holidays, holiday_links = holiday_links,
    events = events, event_links = event_links,
    notices = notices, text_codes = text_codes,
    metadata = metadata
  )

  invisible(semester)
}
