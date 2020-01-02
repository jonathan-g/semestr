
# Package-level globals (idea copied from rmarkdown::render.R)
.globals <- new.env(parent = emptyenv())

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
load_semester_db <- function(db_file, root_crit = NULL) {
  if (is.null(root_crit)) {
    root_crit <- make_root_criteria(".semestr.here",
                                    rprojroot::has_file_pattern("^.*\\.RProj$"),
                                    rprojroot::has_dir(".Rproj.user"),
                                    rprojroot::has_dir("content"))

  } else {
    root_crit <- rprojroot::as.root_criterion(root_crit)
  }

  root_dir <- rprojroot::find_root(root_crit, dirname(db_file))
  slide_dir <- file.path(root_dir, "static", "slides")
  tz <- get_semestr_tz()

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
  assign("metadata", metadata, envir = .globals)

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

  calendar <- calendar %>%
    dplyr::mutate(date = lubridate::as_datetime(date, tz = tz))

  bare_dates <- calendar %>% dplyr::select(cal_id, date)

  duplicates <- purrr::keep(calendar$cal_id, duplicated)
  assertthat::assert_that(is_empty(duplicates),
                          msg = stringr::str_c("Duplicated cal_ids: (",
                                               stringr::str_c(duplicates, collapse = ", "),
                                               ")."))

  due_dates <- due_links %>% dplyr::left_join(due_dates, by = "due_key")

  missing_due_dates <- calendar %>%
    dplyr::filter(cal_type == "due date") %$% cal_id %>%
    setdiff(due_dates$cal_id)
  valid_due_dates <-
    assertthat::assert_that(length(missing_due_dates) == 0,
                            msg = stringr::str_c("Missing due dates: (",
                                                 stringr::str_c(missing_due_dates, collapse = ", "),
                                                 ")."))
  if (! isTRUE(valid_due_dates)) {
    warning(valid_due_dates)
  }

  calendar <- calendar %>%
    dplyr::left_join( dplyr::select(due_dates, cal_id, due_type = type,
                                    due_action = action), by = "cal_id")

  hw_asgt <- hw_asgt %>% dplyr::inner_join(hw_links, by = "hw_key") %>%
    dplyr::left_join(hw_topics, by = "hw_key") %>%
    dplyr::left_join(
      dplyr::select(due_dates, due_key, due_cal_id = cal_id),
      by = "due_key") %>%
    dplyr::mutate_at(dplyr::vars(is_numbered),
                     ~as.logical(.) %>% tidyr::replace_na(FALSE))
  hw_items <- hw_items %>% dplyr::inner_join(hw_links, by = "hw_key") %>%
    dplyr::mutate_at(dplyr::vars(undergraduate_only, graduate_only,
                                 break_before, prologue, epilogue),
                     ~as.logical(.) %>% tidyr::replace_na(FALSE))
  hw_sol <- hw_sol %>% dplyr::inner_join(hw_links, by = "hw_key") %>%
    dplyr::inner_join( dplyr::select(due_dates, sol_pub_key = due_key,
                                     sol_pub_cal_id  = cal_id),
                       by = "sol_pub_key")

  missing_hw <- calendar %>%
    dplyr::filter(cal_type == "homework") %$% cal_id %>%
    setdiff(hw_asgt$cal_id)
  valid_hw <-
    assertthat::assert_that(length(missing_hw) == 0,
                            msg = stringr::str_c("Missing homework assignments: (",
                                                 stringr::str_c(missing_hw, collapse = ", "),
                                                 ")."))
  if (! isTRUE(valid_hw)) {
    warning(valid_hw)
  }

  rd_items <- rd_items %>% dplyr::inner_join(rd_links, by = "rd_key") %>%
    dplyr::left_join(rd_src, by = "src_key") %>%
    dplyr::mutate_at(dplyr::vars(undergraduate_only, graduate_only, optional,
                                 prologue, epilogue, textbook, handout),
                     ~as.logical(.) %>% tidyr::replace_na(FALSE))

  missing_reading <- calendar %>%
    dplyr::filter(cal_type == "class") %$% cal_id %>%
    setdiff(rd_items$cal_id)
  valid_reading <-
    assertthat::validate_that(length(missing_reading) == 0,
                              msg = stringr::str_c("Missing reading assignments: (",
                                                   stringr::str_c(missing_reading,
                                                                  collapse = ", "),
                                                   ")."))
  if (! isTRUE(valid_reading)) {
    warning(valid_reading)
  }


  lab_asgt <- lab_asgt %>% dplyr::inner_join(lab_links, by = "lab_key") %>%
    dplyr::left_join( dplyr::select(due_dates, report_due_key = due_key,
                                    report_cal_id = cal_id),
                      by = "report_due_key") %>%
    dplyr::left_join( dplyr::select(due_dates, presentation_key = due_key,
                                    pres_cal_id = cal_id),
                      by = "presentation_key")

  missing_labs <- calendar %>%
    dplyr::filter(cal_type == "lab") %$% cal_id %>%
    setdiff(lab_asgt$cal_id)
  valid_labs <-
    assertthat::validate_that(length(missing_labs) == 0,
                              msg = stringr::str_c("Missing lab assignments: (",
                                                   stringr::str_c(missing_labs,
                                                                  collapse = ", "),
                                                   ")."))
  if (! isTRUE(valid_labs)) {
    warning(valid_labs)
  }

  lab_items <- lab_items %>% dplyr::inner_join(lab_links, by = "lab_key")
  lab_sol <- lab_sol %>% dplyr::inner_join(lab_links, by = "lab_key") %>%
    dplyr::inner_join( dplyr::select(due_dates, sol_pub_key = due_key,
                                     sol_pub_cal_id  = cal_id),
                       by = "sol_pub_key")

  events <- events %>% dplyr::inner_join(event_links, by = "event_key")

  missing_events <- calendar %>%
    dplyr::filter(cal_type == "event") %$% cal_id %>%
    setdiff(events$cal_id)
  valid_events <-
    assertthat::validate_that(length(missing_events) == 0,
                              msg = stringr::str_c("Missing events: (",
                                                   stringr::str_c(missing_events,
                                                                  collapse = ", "),
                                                   ")."))
  if (! isTRUE(valid_events)) {
    warning(valid_events)
  }

  exams <- exams %>% dplyr::inner_join(exam_links, by = "exam_key")

  missing_exams <- calendar %>%
    dplyr::filter(cal_type == "exam") %$% cal_id %>%
    setdiff(exams$cal_id)
  valid_exams <-
    assertthat::assert_that(length(missing_exams) == 0,
                            msg = stringr::str_c("Missing exams: (",
                                                 stringr::str_c(missing_exams,
                                                                collapse = ", "),
                                                 ")."))
  if (! isTRUE(valid_exams)) {
    warning(valid_exams)
  }


  holidays <- holidays %>% dplyr::inner_join(holiday_links, by = "holiday_key")

  missing_holidays <- calendar %>%
    dplyr::filter(cal_type == "holiday") %$% cal_id %>%
    setdiff(holidays$cal_id)
  valid_holidays <-
    assertthat::assert_that(length(missing_holidays) == 0,
                            msg = stringr::str_c("Missing holidays: (",
                                                 stringr::str_c(missing_holidays,
                                                                collapse = ", "),
                                                 ")."))

  notices <- notices %>%
    dplyr::inner_join( dplyr::select(calendar, cal_id, cal_key),
                       by = "cal_key")

  bad_codes <- text_codes %>% dplyr::filter(is.na(code_value))
  if (nrow(bad_codes) > 0) {
    warning("Text codes with missing values: [",
            str_c(bad_codes$code_name, collapse = ", "), "]")
    text_codes <- text_codes %>%
      dplyr::mutate(code_value = ifelse(is.na(code_value), "", code_value))
  }
  text_codes <- list(
    md = text_codes %>% { set_names(.$code_value, .$code_name) },
    latex = text_codes %>% { set_names(.$latex_value, .$code_name) }
  )

  # Add dates to items
  rd_items <- rd_items %>% dplyr::left_join(bare_dates, by = "cal_id")
  hw_asgt <- hw_asgt %>% dplyr::left_join(bare_dates, by = "cal_id") %>%
    dplyr::left_join( dplyr::rename(bare_dates, due_cal_id = cal_id,
                                   due_date = date), by = "due_cal_id")
  hw_items <- hw_items %>% dplyr::left_join(bare_dates, by = "cal_id")
  lab_asgt <- lab_asgt %>% dplyr::left_join(bare_dates, by = "cal_id") %>%
    dplyr::left_join( dplyr::rename(bare_dates, report_cal_id = cal_id,
                                    report_date = date), by = "report_cal_id") %>%
    dplyr::left_join( dplyr::rename(bare_dates, pres_cal_id = cal_id,
                                    pres_date = date), by = "pres_cal_id")
  lab_items <- lab_items %>% dplyr::left_join(bare_dates, by = "cal_id")

  hw_sol <- hw_sol %>% dplyr::left_join(bare_dates, by = "cal_id") %>%
    dplyr::left_join( dplyr::select(bare_dates, sol_pub_cal_id = cal_id,
                                    sol_pub_date = date), by = "sol_pub_cal_id")
  lab_sol <- lab_sol %>% dplyr::left_join(bare_dates, by = "cal_id") %>%
    dplyr::left_join( dplyr::select(bare_dates, sol_pub_cal_id = cal_id,
                                    sol_pub_date = date), by = "sol_pub_cal_id")

  holidays <- holidays  %>% dplyr::left_join(bare_dates, by = "cal_id")
  exams <- exams  %>% dplyr::left_join(bare_dates, by = "cal_id")
  events <- events  %>% dplyr::left_join(bare_dates, by = "cal_id")

  rd_items <- rd_items %>% dplyr::mutate(cal_key = add_key_prefix(rd_key, "class"))
  rd_links <- rd_links %>% dplyr::mutate(cal_key = add_key_prefix(rd_key, "class"))

  hw_asgt <- hw_asgt %>% dplyr::mutate(cal_key = add_key_prefix(hw_key, "homework"))
  hw_items <- hw_items %>% dplyr::mutate(cal_key = add_key_prefix(hw_key, "homework"))
  hw_sol <- hw_sol %>% dplyr::mutate(cal_key = add_key_prefix(hw_key, "homework"),
                                     pub_cal_key = add_key_prefix(sol_pub_key, "due date"))
  hw_links <- hw_links %>% dplyr::mutate(cal_key = add_key_prefix(hw_key, "homework"))

  lab_asgt <- lab_asgt %>% dplyr::mutate(cal_key = add_key_prefix(lab_key, "lab"))
  lab_items <- lab_items %>% dplyr::mutate(cal_key = add_key_prefix(lab_key, "lab"))
  lab_sol <- lab_sol %>% dplyr::mutate(cal_key = add_key_prefix(lab_key, "lab"),
                                     pub_cal_key = add_key_prefix(sol_pub_key, "due date"))
  lab_links <- lab_links %>% dplyr::mutate(cal_key = add_key_prefix(lab_key, "lab"))

  holidays <- holidays %>% dplyr::mutate(cal_key = add_key_prefix(holiday_key, "holiday"))
  exams <- exams %>% dplyr::mutate(cal_key = add_key_prefix(exam_key, "exam"))
  events <- events %>% dplyr::mutate(cal_key = add_key_prefix(event_key, "event"))

  first_class <- 1
  last_class <- NA

  if(is.na(first_class)) first_class <- min(calendar$class_num, na.rm = T)
  if (is.na(last_class)) last_class <- max(calendar$class_num, na.rm = T)

  first_date <- calendar %>%
    dplyr::filter(class_num == first_class) %$% date
  last_date <- calendar %>%
    dplyr::filter(class_num == last_class) %$% date

  year_taught <- lubridate::year(first_date)

  # Pub date should be last day of previous month.
  pub_date <- make_pub_date(first_date, tz)

  semester_dates <- list(
    first_class = first_class, last_class = last_class,
    first_date = first_date, last_date = last_date,
    year_taught = year_taught, pub_date = pub_date
  )

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
    metadata = metadata, semester_dates = semester_dates,
    tz = tz, root_dir = root_dir, slide_dir = slide_dir
  )

  assign("metadata", metadata, envir = .globals)
  assign("semester_dates", semester_dates, envir = .globals)
  assign("semester_data", semester, envir = .globals)
  assign("root_dir", root_dir, envir = .globals)
  assign("slide_dir", slide_dir, envir = .globals)
  assign("tz", tz, envir = .globals)

  invisible(semester)
}
