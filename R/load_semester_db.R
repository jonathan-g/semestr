# Package-level globals (idea copied from rmarkdown::render.R)
.globals <- new.env(parent = emptyenv())

set_up_due_dates <- function(calendar, due_dates, link_cal_due) {
  has_due_dates <- ! (is.null(due_dates) || is.null(link_cal_due))

  if (has_due_dates) {
    due_dates <- link_cal_due %>% dplyr::left_join(due_dates, by = "due_id") %>%
      dplyr::filter(! is.na(.data$due_key), ! is.na(.data$cal_id))

    missing_due_dates <- calendar %>%
      dplyr::filter(.data$cal_type == "due date") %>%
      dplyr::pull("cal_id") %>%
      setdiff(due_dates$cal_id)
    valid_due_dates <- assertthat::validate_that(
      length(missing_due_dates) == 0,
      msg = stringr::str_c("Missing due dates: (",
                           stringr::str_c(missing_due_dates, collapse = ", "),
                           ").")
    )
    if (! isTRUE(valid_due_dates)) {
      warning(valid_due_dates)
    }
    calendar <- calendar %>%
      dplyr::left_join(
        dplyr::select(due_dates, "cal_id", "due_type", "due_action"),
        by = "cal_id"
      )
  } else {
    due_dates <- tibble::tibble(
      cal_id = integer(0), due_id = integer(0), due_key = character(0),
      due_desc = character(0), due_action = character(0),
      due_type = character(0)
    )
    missing_due_dates <- NULL
    calendar <- dplyr::mutate(calendar, due_type = NA_character_,
                              due_action = NA_character_)
  }

  invisible(
    list(has_due_dates <- has_due_dates,
         calendar = calendar,
         due_dates = due_dates,
         missing_due_dates = missing_due_dates)
  )
}

set_up_reading_sources <- function(reading_sources, handouts,
                                   file_paths) {
  has_reading_sources <- ! (is.null(reading_sources))
  has_handouts <- ! (is.null(handouts))
  reading_sources <- dplyr::mutate(reading_sources,
    dplyr::across(c('textbook', 'handout', 'web_page', 'youtube'),
                  as.logical)
  )
  if (has_reading_sources && has_handouts) {
    missing_handouts <- reading_sources %>% dplyr::filter(
      ! is.na(.data$handout_key),
      ! .data$handout_key %in% handouts$handout_key
    )
    missing_handouts_2 <- reading_sources %>% dplyr::filter(
      .data$handout, is.na(.data$handout_key), is.na(.data$url)
    )
    external_handouts <- reading_sources %>% dplyr::filter(
      .data$handout, is.na(.data$handout_key), ! is.na(.data$url)
      )
    internal_handouts <- external_handouts %>% dplyr::filter(
      ! stringr::str_detect(.data$url, "^http")
    )
    ambi_handouts <- reading_sources %>% dplyr::filter(
      !is.na(.data$handout_key), !is.na(.data$url)
    )
    if (nrow(missing_handouts) > 0) {
      warning("WARNING: Missing handouts: (",
              stringr::str_c(missing_handouts$src_key, collapse = ", "),
              ")")
    }
    if (nrow(missing_handouts_2) > 0) {
      warning("WARNING: Handouts don't have key or URL: (",
              stringr::str_c(missing_handouts_2$src_key,
                             collapse = ", "),
              ")")
    }
    if (nrow(internal_handouts) > 0) {
      warning("WARNING: Handouts have internal URL instead of key: (",
      stringr::str_c(internal_handouts$src_key, collapse = ", "),
      ")")
    }
    if (nrow(ambi_handouts) > 0) {
      warning("WARNING: Handouts have both URL and key: (",
              stringr::str_c(ambi_handouts$src_key, collapse = ", "),
              ")")
    }
    reading_sources <- reading_sources %>%
      dplyr::left_join(
        dplyr::select(handouts,
               'handout_key', 'doc_title', 'doc_short_title',
               'doc_markdown_title', 'doc_short_markdown_title',
               'doc_latex_title', 'doc_short_latex_title',
               'doc_citation', 'doc_slug'),
        by = "handout_key") %>%
      dplyr::mutate(
        title =
          ifelse(is.na(.data$handout_key),
                 .data$title, .data$doc_title),
        short_title =
          ifelse(is.na(.data$handout_key),
                 .data$short_title, .data$doc_short_title),
        markdown_title =
          ifelse(is.na(.data$handout_key),
                 .data$markdown_title, .data$doc_markdown_title),
        short_markdown_title =
          ifelse(is.na(.data$handout_key),
                 .data$short_markdown_title,
                 .data$doc_short_markdown_title),
        latex_title =
          ifelse(is.na(.data$handout_key),
                 .data$latex_title, .data$doc_latex_title),
        short_latex_title =
          ifelse(is.na(.data$handout_key),
                 .data$short_latex_title, .data$doc_short_latex_title),
        citation =
          ifelse(is.na(.data$handout_key),
                 .data$citation, .data$doc_citation),
        url =
          ifelse(is.na(.data$handout_key), .data$url,
                 file.path(file_paths['handout_dest'], .data$doc_slug) %>%
                   clean_url())
      ) %>%
      dplyr::select(-'doc_title', -'doc_short_title',
             -'doc_markdown_title', -'doc_short_markdown_title',
             -'doc_latex_title', -'doc_short_latex_title',
             -'doc_citation', -'doc_slug')
  }

  invisible(
    list(
      reading_sources = reading_sources,
      handouts = handouts
    )
  )

}

set_up_reading <- function(calendar, classes, reading_items,
                          reading_groups, reading_sources,
                          link_cls_rd, link_cal_class) {
  has_reading <- ! (is.null(reading_items) || is.null(link_cls_rd))
  if (has_reading) {
    class_df <- dplyr::inner_join(
      dplyr::select(classes, "class_id", "class_key"),
      dplyr::select(link_cal_class, "cal_id", "class_id",
                    key_chk = "class_key"),
      by = "class_id"
    )
    class_mismatches <- class_df %>%
      dplyr::filter(.data$class_key != .data$key_chk)
    if (nrow(class_mismatches) > 0) {
      warning("WARNING: class links mismatch on ",
              stringr::str_c(class_mismatches$key_chk, collapse = ", "),
              ".")
    }
    class_df <- class_df %>% dplyr::select(-"key_chk")

    rd_items <- reading_items %>%
      dplyr::inner_join(
        dplyr::select(reading_groups, "rd_grp_id", "rd_grp_key"),
        by = "rd_grp_key"
      ) %>%
      dplyr::inner_join(link_cls_rd, by = "rd_grp_key") %>%
      dplyr::inner_join(class_df, by = "class_key") %>%
      dplyr::left_join(reading_sources, by = "src_key") %>%
      dplyr::mutate(dplyr::across(
        c("undergraduate_only", "graduate_only",
          "optional", "textbook", "handout", "web_page", "youtube",
          "rd_prologue", "rd_epilogue", "rd_break_before"),
        ~as.logical(.x) %>%
          tidyr::replace_na(FALSE))
      )
    missing_reading <- calendar %>%
      dplyr::filter(.data$cal_type == "class") %>%
      dplyr::pull("cal_id") %>%
      setdiff(rd_items$cal_id)
    valid_reading <- assertthat::validate_that(
      length(missing_reading) == 0,
      msg = stringr::str_c("Missing reading assignments: (",
                           stringr::str_c(missing_reading, collapse = ", "),
                           ").")
    )
    if (! isTRUE(valid_reading)) {
      warning(valid_reading)
    }
  } else {
    rd_items <- NULL
    link_cls_rd <- NULL
    missing_reading <- NULL
    class_mismatches <- NULL
  }

  invisible(
    list(
      has_reading = has_reading,
      calendar = calendar,
      rd_items = rd_items,
      missing_reading = missing_reading,
      class_mismatches = class_mismatches
    )
  )
}

set_up_handouts <- function(calendar, handouts, reading_sources) {
  has_handouts <- ! (is.null(handouts))
  if (has_handouts) {
    missing_handouts <- reading_sources %>%
      dplyr::filter( ! is.na(.data$handout_key),
              ! .data$handout_key %in% handouts$handout_key
              )
    valid_handouts <- assertthat::validate_that(
      nrow(missing_handouts) == 0,
      msg = stringr::str_c("Missing handouts in reading sources: (",
                           stringr::str_c(
                             "Handout",
                             missing_handouts$handout_key,
                             "in reading source",
                             missing_handouts$src_key,
                             sep = " ", collapse = ", "),
                           ").")
    )
  } else {
    handouts <- NULL
    missing_handouts <- NULL
  }

  invisible(
    list(
      has_handouts = has_handouts,
      calendar = calendar,
      handouts = handouts,
      missing_handouts = missing_handouts
    )
  )
}

set_up_homework <- function(calendar, homework_assignments,
                            homework_topics, homework_groups,
                            homework_items, homework_solutions,
                            due_dates, link_cal_hw) {
  has_homework <- ! (is.null(homework_assignments) || is.null(link_cal_hw))
  if (has_homework) {
    hw_asgt <- homework_assignments %>%
      dplyr::inner_join(homework_groups, by = "hw_grp_key") %>%
      dplyr::inner_join(link_cal_hw, by = "hw_grp_id") %>%
      dplyr::left_join(homework_topics, by = "hw_grp_key") %>%
      dplyr::left_join(
        dplyr::select(due_dates, "due_key", due_cal_id = "cal_id"),
        by = c(hw_due_key = "due_key")) %>%
      dplyr::mutate(
        dplyr::across(c("hw_is_numbered", "uses_gh_classroom"),
                      ~as.logical(.x) %>% tidyr::replace_na(FALSE))
      ) %>%
      dplyr::arrange(.data$hw_grp_order) %>%
      dplyr::mutate(hw_num = seq(dplyr::n())) %>%
      dplyr::select(-"hw_grp_order")

    if (!has_name(homework_items, "hw_self_assess")) {
      warning("Database table homework_items is missing column hw_self_assess.",
              "Setting to default of FALSE.")
      homework_items <- homework_items %>% dplyr::mutate(hw_self_assess = FALSE)
    }
    if (!has_name(homework_items, "hw_optional")) {
      warning("Database table homework_items is missing column hw_optional.",
              "Setting to default of FALSE.")
      homework_items <- homework_items %>% dplyr::mutate(hw_optional = FALSE)
    }
    hw_items <- homework_items %>% dplyr::inner_join(
      dplyr::select(homework_groups, "hw_grp_id", "hw_grp_key"),
      by = "hw_grp_key"
    ) %>%
      dplyr::inner_join(link_cal_hw, by = "hw_grp_id") %>%
      dplyr::left_join(dplyr::select(hw_asgt, "hw_grp_id", "hw_num"),
                       by = "hw_grp_id") %>%
      dplyr::mutate(dplyr::across(c("undergraduate_only", "graduate_only",
                                    "hw_self_assess", "hw_optional",
                                    "hw_break_before", "hw_prologue",
                                    "hw_epilogue"),
                                  ~as.logical(.x) %>% tidyr::replace_na(FALSE)))
    if (!is.null(homework_solutions)) {
      hw_sol <- homework_solutions %>%
        dplyr::inner_join(
          dplyr::select(homework_groups, "hw_grp_id", "hw_grp_key"),
          by = c(sol_grp_key = "hw_grp_key")
        ) %>%
        dplyr::inner_join(link_cal_hw, by = "hw_grp_id") %>%
        dplyr::left_join(dplyr::select(hw_asgt, "hw_grp_id", "hw_num"),
                         by = "hw_grp_id") %>%
        dplyr::inner_join( dplyr::select(due_dates, sol_pub_key = "due_key",
                                         sol_pub_cal_id = "cal_id"),
                           by = "sol_pub_key")
    } else {
      hw_sol <- NULL
    }

    missing_hw <- calendar %>%
      dplyr::filter(.data$cal_type == "homework") %>%
      dplyr::pull("cal_id") %>%
      setdiff(hw_asgt$cal_id)
    valid_hw <- assertthat::validate_that(
      length(missing_hw) == 0,
      msg = stringr::str_c("Missing homework assignments: (",
                           stringr::str_c(missing_hw, collapse = ", "), ").")
    )
    if (! isTRUE(valid_hw)) {
      warning(valid_hw)
    }
  } else {
    hw_asgt <- NULL
    hw_items <- NULL
    hw_sol <- NULL
    missing_hw <- NULL
  }

  invisible(
    list(
      has_homework = has_homework,
      calendar = calendar,
      hw_asgt = hw_asgt,
      hw_items = hw_items,
      hw_sol = hw_sol,
      missing_hw = missing_hw
    )
  )
}

set_up_labs <- function(calendar, lab_assignments, lab_groups,
                        lab_items, lab_solutions, due_dates,
                        link_cal_lab) {

  has_labs <- ! (is.null(lab_assignments) || is.null(link_cal_lab))
  if (has_labs) {
    lab_asgt <- lab_assignments %>%
      dplyr::inner_join(lab_groups, by = "lab_grp_key") %>%
      dplyr::inner_join(link_cal_lab, by = "lab_grp_id") %>%
      dplyr::left_join(dplyr::select(due_dates, report_due_key = "due_key",
                                     report_cal_id = "cal_id"),
                       by = "report_due_key") %>%
      dplyr::left_join( dplyr::select(due_dates, presentation_key = "due_key",
                                      pres_cal_id = "cal_id"),
                        by = "presentation_key") %>%
      dplyr::arrange(.data$lab_grp_order) %>%
      dplyr::mutate(lab_num = seq(dplyr::n()),
                    uses_gh_classroom = as.logical(.data$uses_gh_classroom) %>%
                      tidyr::replace_na(FALSE)) %>%
      dplyr::select(-"lab_grp_order")

    missing_labs <- calendar %>%
      dplyr::filter(.data$cal_type == "lab") %>%
      dplyr::pull("cal_id") %>%
      setdiff(lab_asgt$cal_id)
    valid_labs <- assertthat::validate_that(
      length(missing_labs) == 0,
      msg = stringr::str_c("Missing lab assignments: (",
                           stringr::str_c(missing_labs, collapse = ", "),
                           ").")
    )
    if (! isTRUE(valid_labs)) {
      warning(valid_labs)
    }

    lab_items <- lab_items %>%
      dplyr::inner_join(dplyr::select(lab_groups, "lab_grp_id", "lab_grp_key"),
                        by = "lab_grp_key") %>%
      dplyr::left_join(dplyr::select(lab_asgt, "lab_grp_id", "lab_num"),
                       by = "lab_grp_id") %>%
      dplyr::inner_join(link_cal_lab, by = "lab_grp_id")
    lab_sol <- lab_solutions %>%
      dplyr::inner_join(dplyr::select(lab_groups, "lab_grp_id", "lab_grp_key"),
                        by = "lab_grp_key") %>%
      dplyr::inner_join(link_cal_lab, by = "lab_grp_id") %>%
      dplyr::left_join(dplyr::select(lab_asgt, "lab_grp_id", "lab_num",
                                     "report_due_key"),
                       by = "lab_grp_id") %>%
      dplyr::inner_join(dplyr::select(due_dates, lab_sol_pub_key = "due_key",
                                      sol_pub_cal_id  = "cal_id"),
                        by = "lab_sol_pub_key") %>%
      dplyr::inner_join(dplyr::select(due_dates, report_due_key = "due_key",
                                      report_due_cal_id = "cal_id"),
                        by = "report_due_key")
  } else {
    lab_asgt <- NULL
    lab_items <- NULL
    lab_sol <- NULL
    missing_labs <- NULL
  }

  invisible(
    list (
      has_labs = has_labs,
      calendar = calendar,
      lab_asgt = lab_asgt,
      lab_items = lab_items,
      lab_sol = lab_sol,
      missing_labs = missing_labs
    )
  )
}

set_up_events <- function(calendar, events, link_cal_event) {
  has_events <- ! (is.null(events) || is.null(link_cal_event))
  if (has_events) {
    events <- events %>%
      dplyr::inner_join(link_cal_event, by = "event_id")

    missing_events <- calendar %>%
      dplyr::filter(.data$cal_type == "event") %>%
      dplyr::pull("cal_id") %>%
      setdiff(events$cal_id)
    valid_events <- assertthat::validate_that(
      length(missing_events) == 0,
      msg = stringr::str_c("Missing events: (",
                           stringr::str_c(missing_events, collapse = ", "),
                           ").")
    )
    if (! isTRUE(valid_events)) {
      warning(valid_events)
    }
  } else {
    events <- NULL
    missing_events <- NULL
  }

  invisible(
    list(
      has_events = has_events,
      calendar = calendar,
      events = events,
      missing_events = missing_events
    )
  )

}

set_up_exams <- function(calendar, exams, link_cal_exam) {
  has_exams <- ! (is.null(exams) || is.null(link_cal_exam))
  if (has_exams) {
    exams <- exams %>% dplyr::inner_join(link_cal_exam, by = "exam_id")

    missing_exams <- calendar %>%
      dplyr::filter(.data$cal_type == "exam") %>%
      dplyr::pull("cal_id") %>%
      setdiff(exams$cal_id)
    valid_exams <- assertthat::assert_that(
      length(missing_exams) == 0,
      msg = stringr::str_c("Missing exams: (",
                           stringr::str_c(missing_exams, collapse = ", "),
                           ").")
    )
    if (! isTRUE(valid_exams)) {
      warning(valid_exams)
    }
  } else {
    exams <- NULL
    link_cal_exam <- NULL
    missing_exams <- NULL
  }

  invisible(
    list(
      has_exams = has_exams,
      calendar = calendar,
      exams = exams,
      missing_exams = missing_exams
    )
  )
}

set_up_holidays <- function(calendar, holidays, link_cal_holiday) {
  has_holidays <- ! (is.null(holidays) || is.null(link_cal_holiday))
  if (has_holidays) {
    holidays <- holidays %>%
      dplyr::inner_join(link_cal_holiday, by = "holiday_id")

    missing_holidays <- calendar %>%
      dplyr::filter(.data$cal_type == "holiday") %>%
      dplyr::pull("cal_id") %>%
      setdiff(holidays$cal_id)
    valid_holidays <- assertthat::assert_that(
      length(missing_holidays) == 0,
      msg = stringr::str_c(
        "Missing holidays: (",
        stringr::str_c(missing_holidays, collapse = ", "),
        ")."
      )
    )

    if (! isTRUE(valid_holidays)) {
      warning(valid_holidays)
    }
  } else {
    holidays <- NULL
    link_cal_holiday <- NULL
    missing_holidays <- NULL
  }

  invisible(
    list(
      has_holidays = has_holidays,
      calendar = calendar,
      holidays = holidays,
      missing_holidays = missing_holidays
    )
  )
}

set_up_text_codes <- function(text_codes) {
  bad_codes <- text_codes %>% dplyr::filter(is.na(.data$code_value))
  if (nrow(bad_codes) > 0) {
    warning("Text codes with missing values: [",
            stringr::str_c(bad_codes$code_name, collapse = ", "), "]")
    text_codes <- text_codes %>%
      dplyr::mutate(code_value = tidyr::replace_na(.data$code_value, ""))

  }
  text_codes <- list(
    md = text_codes %>% { purrr::set_names(.$code_value, .$code_name) },
    latex = text_codes %>% { purrr::set_names(.$latex_value, .$code_name) }
  )

  invisible(
    list(
      text_codes = text_codes,
      bad_codes = bad_codes
    )
  )
}

read_raw_db <- function(db_file, target_env = parent.frame()) {
  # Read tables from database  =========================================

  db <- DBI::dbConnect(RSQLite::SQLite(), db_file)

  # Read metadata tables ===============================================

  message("Tables = ", stringr::str_c(DBI::dbListTables(db), collapse = ", "))

  md_1 <- dplyr::tbl(db, "metadata") %>% dplyr::collect()
  md_2 <- dplyr::tbl(db, "base_mods") %>% dplyr::collect()

  db_config <- dplyr::tbl(db, "config") %>% dplyr::collect() %>%
    dplyr::mutate(value = as.logical(.data$value)) %>%
    { set_names(.$value, .$key) }
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
  assign("metadata", metadata, envir = .globals)
  assign("metadata", metadata, envir = target_env)

  # Read syllabus tables ===============================================

  # This is just to avoid R CMD check complaining about unbound variables.
  calendar <- NULL
  classes <- NULL
  due_dates <- NULL
  events <- NULL
  exams <- NULL
  holidays <- NULL
  notices <- NULL
  link_cal_class <- NULL
  link_cal_due <- NULL
  link_cal_event <- NULL
  link_cal_exam <- NULL
  link_cal_holiday <- NULL
  link_cal_hw <- NULL
  link_cal_lab <- NULL
  link_cls_rd <- NULL
  homework_assignments <- NULL
  homework_groups <- NULL
  homework_items <- NULL
  homework_solutions <- NULL
  homework_topics <- NULL
  lab_assignments <- NULL
  lab_groups <- NULL
  lab_items <- NULL
  lab_solutions <- NULL
  reading_groups <- NULL
  reading_items <- NULL
  reading_sources <- NULL
  handouts <- NULL
  text_codes <- NULL

  for (t in c("calendar", "classes", "due_dates",
              "events", "exams", "holidays", "notices",
              "link_cal_class", "link_cal_due", "link_cal_event",
              "link_cal_exam", "link_cal_holiday", "link_cal_hw",
              "link_cal_lab", "link_cls_rd",
              "homework_assignments", "homework_groups", "homework_items",
              "homework_solutions", "homework_topics",
              "lab_assignments", "lab_groups", "lab_items", "lab_solutions",
              "reading_groups", "reading_items", "reading_sources",
              "handouts",
              "file_paths", "text_codes")) {
    if (DBI::dbExistsTable(db, t)) {
      df <- dplyr::tbl(db, t) %>% dplyr::collect()
      if (nrow(df) == 0) {
        df <- NULL
        warning("Database table ", t, " is empty.")
      }
    } else {
      df <- NULL
      warning("Database has no table ", t)
    }
    assign(t, df, envir = target_env)
  }

  DBI::dbDisconnect(db)

  NULL
}

#' Load schedule for semester from database
#'
#' Loads schedule for class meetings, reading and homework assignments,
#' lab sessions, exams, holidays, etc. from a database.
#'
#' @param db_file An SQLite database file.
#' @param root_crit Criteria for `rprojroot` to use in finding the project
#'   root directory.
#' @param ignore_root Ignore the root criteria and work from the current
#'   directory.
#'
#' @return A list containing a tibbles with the calendar for the semester and
#'   tibbles with details on reading assignments, homework assignments,
#'   labs, exams, holidays, and other events.
#'
#'   The list contains the following tibbles:
#'   `calendar`, `due_dates`, `rd_items`, `rd_src`, `handouts`,
#'   `class_topics`, `hw_asgt`, `hw_items`, `hw_sol`, `hw_topics`,
#'   `lab_asgt`, `lab_items`, `lab_sol`, `exams`,
#'   `holidays`, `events`, `notices`,
#'   `has_reading`, `has_handouts`, `has_homework`, `has_labs`,
#'   `has_exams`, `has_holidays`, `has_events`, `has_notices`,
#'   `file_paths`, `text_codes`,
#'   `metadata`, `semester_dates`, `tz`, `root_dir`, `slide_dir`
#'
#'   and a named list `metadata` containing named character vectors of
#'   metadata that are used to decode and manipulate calendar entries.
#'
#' @export
load_semester_db <- function(db_file, root_crit = NULL, ignore_root = FALSE) {
  if (ignore_root) {
    root_dir <- getwd()
  } else {
    if (is.null(root_crit)) {
      root_crit <- make_root_criteria(".semestr.here",
                                      rprojroot::has_file_pattern("^.*\\.RProj$"),
                                      rprojroot::has_dir(".Rproj.user"),
                                      rprojroot::has_dir("content"))

    } else {
      root_crit <- rprojroot::as.root_criterion(root_crit)
    }

    root_dir <- rprojroot::find_root(root_crit, dirname(db_file))
  }
  slide_dir <- file.path(root_dir, "static", "slides")
  tz <- get_semestr_tz()

  # Dummy to prevent R CMD check complaining ===========================
  # about undeclared variables
  #
  calendar <- NULL
  classes <- NULL
  due_dates <- NULL
  events <- NULL
  exams <- NULL
  holidays <- NULL
  notices <- NULL
  link_cal_class <- NULL
  link_cal_due <- NULL
  link_cal_event <- NULL
  link_cal_exam <- NULL
  link_cal_holiday <- NULL
  link_cal_hw <- NULL
  link_cal_lab <- NULL
  link_cls_rd <- NULL
  homework_assignments <- NULL
  homework_groups <- NULL
  homework_items <- NULL
  homework_solutions <- NULL
  homework_topics <- NULL
  lab_assignments <- NULL
  lab_groups <- NULL
  lab_items <- NULL
  lab_solutions <- NULL
  reading_groups <- NULL
  reading_items <- NULL
  reading_sources <- NULL
  handouts <- NULL
  text_codes <- NULL
  metadata <- NULL

  # Read database ======================================================
  read_raw_db(db_file)

  # Set up file paths for pdfs, etc. ===================================
  file_paths <- with(file_paths, purrr::set_names(path, name))
  default_file_paths <- c(
    rd_asgt_pdf = '/files/reading_asgts/',
    hw_asgt_pdf = '/files/homework_asgts/',
    lab_doc_pdf = '/files/lab_docs/',
    handout_pdf = '/files/handouts/',
    hw_sol_pdf = '/files/homework_solutions/',
    lab_sol_pdf = '/files/lab_solutions/',
    hw_sol_src = '/content/homework_solutions/',
    hw_asgt_src = '/content/assignment',
    hw_sol_dest = '/homework_solutions/',
    rd_asgt_src = '/content/reading/',
    rd_asgt_dest = '/reading/',
    lab_doc_dest = '/lab_docs/',
    hw_asgt_dest = '/assignment/',
    lab_doc_src = '/content/lab_docs/',
    lab_sol_src = '/content/lab_solutions/',
    lab_sol_dest = '/lab_solutions/',
    handout_src = '/content/handouts',
    handout_dest = '/handouts',
    lab_asgt_src = '/content/labs/',
    lab_asgt_dest = '/labs/',
    lab_asgt_pdf = '/files/labs/'
  )

  i <- setdiff(names(default_file_paths), names(file_paths))
  file_paths[i] <- default_file_paths[i]

  # Start setting up master calendar for the semester ==================

  calendar <- calendar %>%
    dplyr::mutate(date = lubridate::as_datetime(.data$date, tz = tz),
                  cal_type = item_type(.data$cal_id),
                  cal_id = as.integer(.data$cal_id)) %>%
    dplyr::filter(! is.na(.data$date))

  bare_dates <- calendar %>% dplyr::select("cal_id", "date")

  duplicates <- purrr::keep(calendar$cal_id, duplicated)
  assertthat::assert_that(
    is_empty(duplicates),
    msg = stringr::str_c("Duplicated cal_ids: (",
                         stringr::str_c(duplicates, collapse = ", "), ").")
  )

  # Set up due dates ===================================================

  tmp <- set_up_due_dates(calendar, due_dates, link_cal_due)
  has_due_dates <- tmp$has_due_dates
  calendar <- tmp$calendar
  due_dates <- tmp$due_dates

  # Set up reading assignments =========================================

  tmp <- set_up_reading_sources(reading_sources, handouts,
                                file_paths)
  reading_sources <- tmp$reading_sources
  handouts <- tmp$handouts

  tmp <- set_up_reading(calendar, classes, reading_items,
                        reading_groups, reading_sources,
                        link_cls_rd, link_cal_class)
  has_reading <- tmp$has_reading
  calendar <- tmp$calendar
  rd_items <- tmp$rd_items

  # Set up handouts ====================================================

  tmp <- set_up_handouts(calendar, handouts, reading_sources)
  has_handouts <- tmp$has_handouts
  calendar <- tmp$calendar
  handouts <- tmp$handouts

  # Set up homework assignments ========================================

  tmp <- set_up_homework(calendar, homework_assignments,
                         homework_topics, homework_groups,
                         homework_items, homework_solutions,
                         due_dates, link_cal_hw)
  has_homework <- tmp$has_homework
  calendar <- tmp$calendar
  hw_asgt <- tmp$hw_asgt
  hw_items <- tmp$hw_items
  hw_sol <- tmp$hw_sol


  # Set up lab assignments =============================================

  tmp <- set_up_labs(calendar, lab_assignments, lab_groups, lab_items,
                     lab_solutions, due_dates, link_cal_lab)
  has_labs <- tmp$has_labs
  calendar <- tmp$calendar
  lab_asgt <- tmp$lab_asgt
  lab_items <- tmp$lab_items
  lab_sol <- tmp$lab_sol


  # Set up events ======================================================

  tmp <- set_up_events(calendar, events, link_cal_event)
  has_events <- tmp$has_events
  calendar <- tmp$calendar
  events <- tmp$events

  # Set up exams =======================================================

  tmp <- set_up_exams(calendar, exams, link_cal_exam)
  has_exams <- tmp$has_exams
  calendar <- tmp$calendar
  exams <- tmp$exams

  # Set up holidays ====================================================

  tmp <- set_up_holidays(calendar, holidays, link_cal_holiday)
  has_holidays <- tmp$has_holidays
  calendar <- tmp$calendar
  holidays <- tmp$holidays

  # Set up notices =====================================================

  has_notices <- ! is.null(notices)

  # Set up text codes ==================================================

  tmp <- set_up_text_codes(text_codes)
  text_codes <- tmp$text_codes

  # Match items (homework, reading, labs, etc.) with dates =============

  if (has_reading) {
    rd_items <- rd_items %>% dplyr::left_join(bare_dates, by = "cal_id")
  }
  if (has_homework) {
    hw_asgt <- hw_asgt %>% dplyr::left_join(bare_dates, by = "cal_id") %>%
      dplyr::left_join( dplyr::rename(bare_dates, due_cal_id = "cal_id",
                                      due_date = "date"), by = "due_cal_id")
    hw_items <- hw_items %>% dplyr::left_join(bare_dates, by = "cal_id")
    if (! is.null(hw_sol)) {
    hw_sol <- hw_sol %>% dplyr::left_join(bare_dates, by = "cal_id") %>%
      dplyr::left_join( dplyr::select(bare_dates, sol_pub_cal_id = "cal_id",
                                      sol_pub_date = "date"),
                        by = "sol_pub_cal_id")
    }
  }
  if (has_handouts) {
    handouts <- handouts %>%
      dplyr::left_join(dplyr::select(rd_items, 'date', 'handout_key'),
                       by = "handout_key") %>%
      dplyr::slice_min(date, n = 1, by = 'handout_key',
                       with_ties = FALSE, na_rm = FALSE)
  }
  if (has_labs) {
    lab_asgt <- lab_asgt %>% dplyr::left_join(bare_dates, by = "cal_id") %>%
      dplyr::left_join(dplyr::rename(bare_dates, report_cal_id = "cal_id",
                                      report_date = "date"),
                        by = "report_cal_id") %>%
      dplyr::left_join(dplyr::rename(bare_dates, pres_cal_id = "cal_id",
                                      pres_date = "date"),
                        by = "pres_cal_id")
    lab_items <- lab_items %>% dplyr::left_join(bare_dates, by = "cal_id")
    lab_sol <- lab_sol %>%
      dplyr::left_join(dplyr::select(bare_dates, "cal_id", lab_date = "date"),
                                     by = "cal_id") %>%
      dplyr::left_join(dplyr::select(bare_dates, sol_pub_cal_id = "cal_id",
                                      sol_pub_date = "date"),
                        by = "sol_pub_cal_id") %>%
      dplyr::left_join(dplyr::select(bare_dates, report_due_cal_id = "cal_id",
                                     report_date = "date"),
                       by = "report_due_cal_id")
  }
  if (has_holidays) {
    holidays <- holidays  %>% dplyr::left_join(bare_dates, by = "cal_id")
  }
  if (has_exams) {
    exams <- exams  %>% dplyr::left_join(bare_dates, by = "cal_id")
  }
  if (has_events) {
    events <- events  %>% dplyr::left_join(bare_dates, by = "cal_id")
  }

  # Set up textual calendar keys =======================================

  if (has_reading) {
    rd_items <- rd_items %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$rd_grp_key, "class"))
  }
  if (has_homework) {
    hw_asgt <- hw_asgt %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$hw_grp_key, "homework"))
    hw_items <- hw_items %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$hw_grp_key, "homework"))
    if (!is.null(hw_sol)) {
    hw_sol <- hw_sol %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$sol_grp_key, "homework"),
                    pub_cal_key = add_key_prefix(.data$sol_pub_key, "due date"))
    }
  }
  if (has_labs) {
    lab_asgt <- lab_asgt %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$lab_grp_key, "lab"))
    lab_items <- lab_items %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$lab_grp_key, "lab"))
    lab_sol <- lab_sol %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$lab_grp_key, "lab"),
                    pub_cal_key = add_key_prefix(.data$lab_sol_pub_key, "due date"))
  }
  if (has_holidays) {
    holidays <- holidays %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$holiday_key, "holiday"))
  }
  if (has_exams) {
    exams <- exams %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$exam_key, "exam"))
  }
  if (has_events) {
    events <- events %>%
      dplyr::mutate(cal_key = add_key_prefix(.data$event_key, "event"))
  }

  # Add cal_key to calendar ============================================

  class_topics <- calendar %>%
    dplyr::filter(.data$cal_type == "class") %>%
    dplyr::select("cal_id") %>%
    dplyr::left_join(dplyr::select(link_cal_class, -"class_key"),
                     by = "cal_id") %>%
    dplyr::left_join(dplyr::select(classes, "class_id", "class_key",
                                   topic = "class_title"),
                     by = "class_id")
  if (has_reading) {
    class_topics <- class_topics %>%
      dplyr::left_join(link_cls_rd, by = "class_key")
  }
  class_topics <- class_topics %>%
    dplyr::select(-"class_id") %>%
    dplyr::rename(cal_key = "class_key") %>%
    add_key_prefix("class")

  cal_keys <- dplyr::bind_rows(
    purrr::map(list(class_topics, hw_asgt, hw_items, hw_sol, lab_asgt,
                    lab_items, lab_sol, holidays, exams, events) %>%
                 purrr::discard(is.null),
               ~dplyr::select(.x, "cal_id", "cal_key"))
  ) %>%
    dplyr::distinct()

  calendar <- calendar %>%
    dplyr::left_join(cal_keys, by = "cal_id") %>%
    dplyr::left_join(link_cal_class, by = "cal_id")

  # Set up class schedule ==============================================

  first_class <- 1
  last_class <- NA

  if(is.na(first_class)) first_class <- min(calendar$class_num, na.rm = T)
  if (is.na(last_class)) last_class <- max(calendar$class_num, na.rm = T)

  first_date <- calendar %>%
    dplyr::filter(.data$class_num == first_class) %$% date
  last_date <- calendar %>%
    dplyr::filter(.data$class_num == last_class) %$% date

  year_taught <- lubridate::year(first_date)

  # Pub date should be last day of previous month.
  pub_date <- make_pub_date(first_date, tz)

  semester_dates <- list(
    first_class = first_class, last_class = last_class,
    first_date = first_date, last_date = last_date,
    year_taught = year_taught, pub_date = pub_date
  )

  # Assemble all the tables into a big list ============================

  semester <- list(
    calendar = calendar, class_topics = class_topics, due_dates = due_dates,
    rd_items = rd_items, rd_src = reading_sources,
    handouts = handouts,
    hw_asgt = hw_asgt, hw_items = hw_items, hw_sol = hw_sol,
    lab_asgt = lab_asgt, lab_items = lab_items, lab_sol = lab_sol,
    exams = exams, holidays = holidays, events = events, notices = notices,
    has_reading = has_reading, has_homework = has_homework,
    has_handouts = has_handouts, has_labs = has_labs,
    has_exams = has_exams, has_holidays = has_holidays,
    has_events = has_events, has_notices = has_notices,
    text_codes = text_codes,
    metadata = metadata, semester_dates = semester_dates,
    tz = tz, root_dir = root_dir, slide_dir = slide_dir,
    file_paths = file_paths
  )

  # Save all of our tables to the .globals environment =================

  semester_data <- semester

  globals <- c("metadata", "semester_dates", "semester_data",
               "root_dir", "slide_dir", "tz", "text_codes")
  for (g in globals) {
    assign(g, get(g), envir = .globals)
  }

  # Return the big list ================================================

  invisible(semester)
}
