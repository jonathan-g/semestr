
#' FUNCTION_TITLE (TODO)
#'
#' FUNCTION_DESCRIPTION  (TODO)
#'
#' @param semester DESCRIPTION  (TODO).
#'
#' @return RETURN_DESCRIPTION  (TODO)
#' @examples
#' # ADD_EXAMPLES_HERE  (TODO)
#' @export
init_schedule <- function(semester) {
  schedule <- semester$calendar %>%
    dplyr::filter(.data$cal_type %in% c("class", "exam", "homework", "lab",
                                        "holiday")) %>%
    dplyr::select(id = "cal_id", "date", key = "cal_key", "cal_type") %>%
    dplyr::mutate(
      # dates might be datetimes, so convert everything to calendar dates.
      date = lubridate::as_date(.data$date, tz = get_semestr_tz()),
      cal_type = type2col(.data$cal_type))
  invisible(schedule)
}


#' FUNCTION_TITLE  (TODO)
#'
#' FUNCTION_DESCRIPTION  (TODO)
#'
#' @param schedule DESCRIPTION  (TODO).
#' @param semester DESCRIPTION  (TODO).
#'
#' @return RETURN_DESCRIPTION  (TODO)
#' @examples
#' # ADD_EXAMPLES_HERE  (TODO)
#' @export
schedule_strip_finals <- function(schedule, semester) {
  final_exams <- schedule %>%
    dplyr::filter(.data$key %in%
                    add_key_prefix(c("FINAL_EXAM", "ALT_FINAL_EXAM"), "exam"))

  schedule <- schedule %>% dplyr::filter(! .data$id %in% final_exams$id)
  list(schedule = schedule, final_exams = final_exams)
}

schedule_add_homework <- function(schedule, semester) {
  hw_due <- semester$due_dates %>%
    dplyr::filter(.data$due_type == "homework",
                  .data$due_action %in% c("homework", "report")) %>%
    dplyr::filter(.data$cal_id %in% semester$calendar$cal_id)

  hw <- semester$hw_asgt %>% dplyr::filter(.data$hw_due_key %in% hw_due$due_key)

  missing_hw <- hw %>%
    dplyr::filter(! (.data$cal_key %in% schedule$key &
                     .data$cal_id %in% schedule$id ))

  missing_hw_entries <- missing_hw %>%
    dplyr::select( key = "hw_grp_key", id = "due_cal_id") %>%
    dplyr::left_join(dplyr::select(semester$calendar, id = "cal_id", "date"),
                     by = c("id")) %>%
    dplyr::mutate(cal_type = "homework",
                  date = lubridate::as_date(.data$date, get_semestr_tz()))

  schedule <- schedule %>% dplyr::bind_rows(missing_hw_entries)
  list(schedule = schedule, hw = hw, hw_due = hw_due, missing_hw = missing_hw)
}

schedule_widen <- function(schedule, final_exams, semester,
                           final_is_take_home = TRUE) {
  has_exams <- semester$has_exams
  has_holidays <- semester$has_holidays

  if (! has_exams) {
    final_exams <- NULL
    final_is_take_home <- FALSE
  }
  topics <- semester$class_topics %>%
    dplyr::select(key_class = "cal_key", "topic")
  if (has_exams) {
    exam_topics <- semester$exams %>%
      dplyr::select(key_exam = "exam_key", topic_exam = "exam") %>%
      add_key_prefix(type = "exam", col = "key_exam")
  }

  class_nums <- semester$calendar %>%
    dplyr::select(id_class = "cal_id", "class_num")

  if (final_is_take_home) {
    message("processing final_exams: (",
            stringr::str_c(names(final_exams), collapse = ", "), "), with ",
            nrow(final_exams), " rows.")
    take_home_exam <- dplyr::top_n(final_exams, 1, wt = .data$date)
    take_home_exam$key <- add_key_prefix("TAKE_HOME_FINAL_EXAM", "exam")
    take_home_exam_topics <- tibble::tibble(key_exam = take_home_exam$key,
                                            topic_exam = "Take-home final exam due")
    exam_topics <- dplyr::bind_rows(exam_topics, take_home_exam_topics)
  }

  if (has_holidays) {
    holiday_topics <- semester$holidays %>%
      dplyr::select(topic_holiday = "holiday_name",
                    key_holiday = "holiday_key") %>%
      add_key_prefix(type = "holiday", col = "key_holiday")
  }

  # Works with pmap:
  # Select columns beginning with "topic", discards NA values and keeps the
  # first non-NA value, or uses NA if all columns are missing values.
  t_topic <- function(...) {
    dots <- list(...)
    cols <- names(dots) %>%
      purrr::keep(~stringr::str_starts(.x, stringr::fixed("topic")))
    dots <- dots[cols]
    res <- purrr::discard(dots, is.na)
    if (length(res) == 0) {
      # message("res is empty.")
      res <- NA_character_
    }
    res[[1]]
  }

  if (has_exams) {
    if (final_is_take_home) {
      final_entries <- take_home_exam
    } else {
      final_entries <- final_exams
    }
    schedule <- schedule %>%
      dplyr::bind_rows(final_entries)
  }
  schedule <- schedule %>%
    dplyr::mutate(page = NA_character_) %>%
    tidyr::pivot_wider(names_from = "cal_type",
                       values_from = c("id", "key", "page")) %>%
    dplyr::select(-dplyr::any_of(c("page_exam", "page_holiday"))) %>%
    dplyr::mutate(page_lecture = NA_character_) %>%
    dplyr::left_join( topics, by = "key_class") %>%
    dplyr::left_join( class_nums, by = "id_class")

  if (has_exams) {
    if (! tibble::has_name(schedule, "key_exam")) {
      schedule <- schedule %>% dplyr::mutate(key_exam = NA_character_)
    }
    schedule <- schedule %>%
      dplyr::left_join( exam_topics, by = "key_exam")
  }
  if (has_holidays) {
    if (! tibble::has_name(schedule, "key_holiday")) {
      schedule <- schedule %>% dplyr::mutate(key_holiday = NA_character_)
    }
    dplyr::left_join( holiday_topics, by = "key_holiday")
  }

  schedule <- schedule %>%
    dplyr::mutate(topic = purrr::pmap_chr(., t_topic)) %>%
    dplyr::select(-dplyr::starts_with("topic_"))

  for (col in get_semestr_metadata()$type2col) {
    key_col <- stringr::str_c("key_", col)
    if (key_col %in% names(schedule)) {
      q_key_col <- enquo(key_col)
      schedule <- strip_key_prefix(schedule, col2type(col), !!q_key_col)
    }
  }

  schedule <- schedule %>%
    dplyr::rename(page_reading = "page_class") %>%
    dplyr::distinct()
  list(schedule = schedule)
}

check_schedule <- function(schedule, semester) {
  sched_check <- schedule %>%
    dplyr::group_by(.data$date, .data$cal_type) %>%
    dplyr::summarize(count = dplyr::n(), .groups = "drop") %>%
    dplyr::filter(.data$count > 1) %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarize(bad_indices = stringr::str_c(.data$cal_type,
                                                  collapse = ", "),
                     .groups = "drop") %>%
    dplyr::mutate(bad_indices = stringr::str_c(.data$date, .data$bad_indices,
                                               sep = ": "))

  assertthat::assert_that(nrow(sched_check) == 0,
                          msg = stringr::str_c(
                            "Multiple assignments per class: ",
                            stringr::str_c(sched_check, collapse = "; ")))
}

set_schedule_globals <- function(schedule, semester) {
  if (exists("calendar", envir = .globals)) {
    if (bindingIsLocked("calendar", .globals)) {
      warning("Unexpected: calendar binding is locked.")
      # unlockBinding("calendar", .globals)
    }
  }
  assign("schedule", semester$calendar, envir = .globals)
  if (exists("schedule", envir = .globals)) {
    if (bindingIsLocked("schedule", .globals)) {
      warning("Unexpected: schedule binding is locked.")
      # unlockBinding("schedule", .globals)
    }
  }
  assign("schedule", schedule, envir = .globals)
  assign("calendar", semester$calendar, envir = .globals)
}

comp_na_f <- function(x, y) {
  tidyr::replace_na(x == y, FALSE)
}

copy_slides <- function(schedule, date, cal_entry, semester) {
  class_num <- cal_entry$class_num
  date <- lubridate::as_date(date)
  slide_dir <- semester$slide_dir

  if (! is.na(class_num)) {
    slide_class_dir <- sprintf("class_%02d", class_num)
    slide_url <- file.path("/slides", slide_class_dir, fsep = "/")

    if (file.exists(file.path(slide_dir, slide_class_dir,
                              "index.html"))) {
      message("HTML slide_url for class ", class_num, " on ",
              as.character(date), " is ", slide_url)
      schedule <- schedule %>%
        dplyr::mutate(page_lecture =
                        ifelse(comp_na_f(.data$class_num, cal_entry$class_num),
                               slide_url, .data$page_lecture))
    } else {
      slides <- list.files(file.path(slide_dir, slide_class_dir),
                           pattern = "*.ppt*")
      if (length(slides) > 0) {
        if (length(slides) == 1) {
          these_slides <- slides[1]
          message("One ppt slide found for class ", class_num, " on ",
                  as.character(date), ": ", these_slides)
        } else {
          slide_df <- tibble::tibble(slide = slides) %>%
            dplyr::mutate(date = file.mtime(
              file.path(slide_dir, slide_class_dir, .data$slide))) %>%
            dplyr::arrange(dplyr::desc(.data$date))
          these_slides <- slide_df$slide[1]
          message(length(slides), " ppt slides found for class ", class_num,
                  " on ", as.character(date),
                  ". Choosing ", these_slides)
        }
        slide_url <- file.path(slide_url, these_slides, fsep = "/") %>%
          URLencode()
        message("slide_url = ", slide_url)
        schedule <- schedule %>%
          dplyr::mutate(page_lecture =
                          ifelse(comp_na_f(.data$class_num, cal_entry$class_num),
                                 .data$slide_url, .data$page_lecture))
      } else {
        message("No slides found for class ", class_num, " on ",
                as.character(date))
      }
    }
  }
  invisible(schedule)
}

build_reading_assignment <- function(schedule, date, cal_entry, semester) {
  date <- lubridate::as_date(date)
  root_dir <- semester$root_dir
  class_num <- cal_entry$class_num
  if (! is.na(cal_entry$id_class) &&
      cal_entry$id_class %in% semester$rd_items$cal_id) {
    message("Making reading page for class #", cal_entry$class_num,
            " on ", as.character(date))
    rd_fname <- sprintf("reading_%02d.Rmd", cal_entry$class_num)
    rd_path <- rd_fname %>% file.path(root_dir, "content", "reading", .)
    rd_url <- rd_fname %>% stringr::str_replace("\\.Rmd$", "") %>%
      file.path("/reading", .)
    rd_page <- make_reading_page(cal_entry$id_class, semester)
    cat(rd_page, file = rd_path)
    schedule <- schedule %>%
      dplyr::mutate(page_reading =
                      ifelse(comp_na_f(class_num, cal_entry$class_num),
                             rd_url, .data$page_reading))
  }
  invisible(schedule)
}

build_hw_assignment <- function(schedule, date, cal_entry, semester) {
  if (! is.na(cal_entry$id_hw)) {
    message("Making homework page for ", cal_entry$key_hw)
    links <- generate_hw_assignment(cal_entry$key_hw, semester, TRUE)
    schedule <- schedule %>%
      dplyr::mutate(page_hw = ifelse(comp_na_f(.data$id_hw, cal_entry$id_hw),
                                     links['url'], .data$page_hw))
  }
  invisible(schedule)
}

build_lab_assignment <- function(schedule, date, cal_entry, semester) {
  if (!is.na(cal_entry$id_lab)) {
    message("Making lab page for lab ", cal_entry$key_lab )
    links <- generate_lab_assignment(cal_entry$key_lab, semester, TRUE)
    schedule <- schedule %>%
      dplyr::mutate(page_lab = ifelse(comp_na_f(.data$id_lab, cal_entry$id_lab),
                                      links['url'], .data$page_lab))
  }
  invisible(schedule)
}

build_assignments <- function(schedule, semester) {
  dates <- schedule$date

  has_labs <- "id_lab" %in% names(schedule)
  has_hw <- "id_homework" %in% names(schedule)
  root_dir <- semester$root_dir
  slide_dir <- semester$slide_dir


  for (d in purrr::discard(dates, is.na)) {
    d = lubridate::as_date(d)
    cal_entry <- schedule %>% dplyr::filter(.data$date == d)
    assertthat::assert_that(nrow(cal_entry) == 1,
                            msg = stringr::str_c("Multiple calendar entries for date ",
                                                 as.character(d), "."))

    class_num <- cal_entry$class_num
    reading_id <- cal_entry$id_class
    reading_key <- cal_entry$key_class
    if (has_hw) {
      hw_id <- cal_entry$id_hw
      hw_key <- cal_entry$key_class
    } else {
      hw_id <- NA
      hw_key <- NA
    }
    if (has_labs) {
      lab_id <- cal_entry$id_lab
      lab_key <- cal_entry$key_lab
    } else {
      lab_id <- NA
      lab_key <- NA
    }

    schedule <- schedule %>% copy_slides(d, cal_entry, semester)
    schedule <- schedule %>%
      build_reading_assignment(d, cal_entry, semester)
    schedule <- schedule %>% build_hw_assignment(d, cal_entry, semester)
    schedule <- schedule %>% build_lab_assignment(d, cal_entry, semester)
  }
  invisible(schedule)
}

#' Prepare schedule from database
#'
#' @param semester A semester object returned from
#' \code{\link{load_semester_db}}.
#'
#' @return A tibble containing a schedule
#' @examples
#' \dontrun{
#' sem <- load_semester_db("foo.sqlite3")
#' sched <- prepare_schedule(sem)
#' }
#' @export
prepare_schedule <- function(semester) {
  schedule <- init_schedule(semester)
  tmp <- schedule_strip_finals(schedule, semester)
  schedule <- tmp$schedule
  final_exams <- tmp$final_exams

  tmp <- schedule %>% schedule_add_homework(semester)
  schedule <- tmp$schedule

  tmp <- schedule_widen(schedule, final_exams, semester, TRUE)
  schedule <- tmp$schedule

  set_schedule_globals(schedule, semester)
  invisible(schedule)
}

#' Generate assignments from database
#'
#' Generate RMarkdown files for reading, homework, and lab  assignments and
#' a lessons.yml file for Hugo to use in making a schedule for a course.
#'
#' @param semester A semester object returned from
#' \code{\link{load_semester_db}}.
#'
#' @return A named list containing the lesson plan in YAML text format and
#'   the semester schedule, as a \code{\link{tibble}}.
#' @examples
#' \dontrun{
#' sem <- load_semester_db("foo.sqlite3")
#' asgts <- generate_assignments(sem)
#' }
#' @export
generate_assignments <- function(semester) {
  schedule <- init_schedule(semester)
  tmp <- schedule_strip_finals(schedule, semester)
  schedule <- tmp$schedule
  final_exams <- tmp$final_exams

  tmp <- schedule %>% schedule_add_homework(semester)
  schedule <- tmp$schedule

  tmp <- schedule_widen(schedule, final_exams, semester, TRUE)
  schedule <- tmp$schedule

  set_schedule_globals(schedule, semester)

  schedule <- build_assignments(schedule, semester)

  message("Done building assignments...")

  context <- list(type = "semester schedule")

  lesson_plan <- schedule %>%
    # dplyr::filter(! event_id %in% c("FINAL_EXAM", "ALT_FINAL_EXAM")) %>%
    dplyr::select(date, title = "topic", reading = "page_reading",
                  assignment = "page_hw", lecture = "page_lecture",
                  lab = "page_lab", "topic") %>%
    dplyr::filter(! is.na(.data$date)) %>%
    dplyr::arrange(.data$date) %>%
    dplyr::mutate(date = as.character(.data$date)) %>%
    purrr::pmap(list) %>%
    purrr::map(~purrr::discard(.x, is.na)) %>%
    list(lessons = .) %>%
    yaml::as.yaml() %>%
    expand_codes(context, semester)

  cat(lesson_plan, file = file.path(semester$root_dir, "data", "lessons.yml"))

  invisible(list(lesson_plan = lesson_plan, schedule = schedule))
}
