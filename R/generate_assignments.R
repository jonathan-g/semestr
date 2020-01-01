init_schedule <- function(semester, metadata) {
  schedule <- semester$calendar %>%
    dplyr::filter(cal_type %in% c("class", "exam", "homework", "lab", "holiday")) %>%
    dplyr::select(id = cal_id, date, key = cal_key, cal_type) %>%
    dplyr::mutate(# dates might be datetimes, so convert everything to calendar
      # dates.
      date = lubridate::as_date(date, tz = metadata$tz),
      cal_type = type2col(cal_type, metadata))
  invisible(schedule)
}

schedule_strip_finals <- function(schedule, semester, metadata) {
  final_exams <- schedule %>%
    dplyr::filter(key %in% add_key_prefix(c("FINAL_EXAM", "ALT_FINAL_EXAM"),
                                          metadata, "exam"))

  schedule <- schedule %>% dplyr::filter(! id %in% final_exams$id)
  list(schedule = schedule, final_exams = final_exams)
}

schedule_add_homework <- function(schedule, semester, metadata) {
  hw_due <- semester$due_dates %>%
    dplyr::filter(type == "homework", action %in% c("homework", "report")) %>%
    dplyr::filter(cal_id %in% semester$calendar$cal_id)

  hw <- semester$hw_asgt %>% dplyr::filter(due_key %in% hw_due$due_key)

  missing_hw <- hw %>%
    dplyr::filter(! (cal_key %in% schedule$key & cal_id %in% schedule$id))

  missing_hw_entries <- missing_hw %>%
    dplyr::select( key = hw_key, id = due_cal_id) %>%
    dplyr::left_join(dplyr::select(semester$calendar, id = cal_id, date),
                     by = c("id")) %>%
    dplyr::mutate(cal_type = "hw",
                  date = lubridate::as_date(date, tz = metadata$tz))

  schedule <- schedule %>% dplyr::bind_rows(missing_hw_entries)
  list(schedule = schedule, hw = hw, hw_due = hw_due, missing_hw = missing_hw)
}

schedule_widen <- function(schedule, final_exams, semester, metadata,
                           final_is_take_home = TRUE) {
  topics <- semester$class_topics %>%
    dplyr::select(key_class = cal_key, topic)
  exam_topics <- semester$exams %>%
    dplyr::select(key_exam = exam_key, topic_exam = exam) %>%
    add_key_prefix(metadata, type = "exam", col = "key_exam")
  class_nums <- semester$calendar %>%
    dplyr::select(id_class = cal_id, class_num)

  if (final_is_take_home) {
    take_home_exam <- dplyr::top_n(final_exams, 1, wt = date)
    take_home_exam$key <- add_key_prefix("TAKE_HOME_FINAL_EXAM", metadata, "exam")
    take_home_exam_topics <- tibble::tibble(key_exam = take_home_exam$key,
                                            topic_exam = "Take-home final exam due")
    exam_topics <- dplyr::bind_rows(exam_topics, take_home_exam_topics)
  }

  holiday_topics <- semester$holidays %>%
    dplyr::select(topic_holiday = holiday_name, key_holiday = holiday_key) %>%
    add_key_prefix(metadata, type = "holiday", col = "key_holiday")

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

  schedule <- schedule %>%
    dplyr::bind_rows(ifelse(final_is_take_home, take_home_exam, final_exams)) %>%
    dplyr::mutate(page = NA_character_) %>%
    tidyr::pivot_wider(names_from = cal_type,
                       values_from = c(id, key, page)) %>%
    dplyr::select(-page_exam, -page_holiday) %>%
    dplyr::mutate(page_lecture = NA_character_) %>%
    dplyr::left_join( topics, by = "key_class") %>%
    dplyr::left_join( class_nums, by = "id_class" ) %>%
    dplyr::left_join( exam_topics, by = "key_exam") %>%
    dplyr::left_join( holiday_topics, by = "key_holiday")

  schedule <- schedule %>% dplyr::mutate(topic = purrr::pmap_chr(., t_topic)) %>%
    dplyr::select(-dplyr::starts_with("topic_"))


  for (col in metadata$type2col) {
    key_col <- stringr::str_c("key_", col)
    if (key_col %in% names(schedule)) {
      q_key_col <- enquo(key_col)
      schedule <- strip_key_prefix(schedule, metadata, col2type(col, metadata),
                                   !!q_key_col)
    }
  }

  schedule <- schedule %>% dplyr::rename(page_reading = page_class)
  schedule <- schedule %>% dplyr::distinct()
  list(schedule = schedule)
}

check_schedule <- function(schedule, semester, metadata) {
  sched_check <- schedule %>% dplyr::group_by(date, cal_type) %>%
    dplyr::summarize(count = dplyr::n()) %>% dplyr::ungroup() %>%
    dplyr::filter(count > 1) %>% dplyr::group_by(date) %>%
    dplyr::summarize(bad_indices = stringr::str_c(cal_type,
                                                  collapse = ", ")) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(bad_indices = stringr::str_c(date, bad_indices,
                                               sep = ": "))

  assertthat::assert_that(nrow(sched_check) == 0,
                          msg = stringr::str_c(
                            "Multiple assignments per class: ",
                            stringr::str_c(sched_check, collapse = "; ")))
}

set_schedule_globals <- function(schedule, semester, metadata) {
  if (exists("calendar", envir = .globals)) {
    if (bindingIsLocked("calendar", .globals)) {
      unlockBinding("calendar", .globals)
    }
  }
  assign("schedule", semester$calendar, envir = .globals)
  if (exists("schedule", envir = .globals)) {
    if (bindingIsLocked("schedule", .globals)) {
      unlockBinding("schedule", .globals)
    }
  }
  assign("schedule", schedule, envir = .globals)
  assign("calendar", semester$calendar, envir = .globals)
}

comp_na_f <- function(x, y) {
  tidyr::replace_na(x == y, FALSE)
}

copy_slides <- function(date, cal_entry, semester, metadata) {
  class_num <- cal_entry$class_num
  date <- lubridate::as_date(date)

  if (! is.na(class_num)) {
    slide_class_dir <- sprintf("class_%02d", class_num)
    slide_url <- file.path("/slides", slide_class_dir, fsep = "/")

    if (file.exists(file.path(slide_dir, slide_class_dir,
                              "index.html"))) {
      message("HTML slide_url for class ", class_num, " on ",
              as.character(date), " is ", slide_url)
      schedule <- schedule %>%
        dplyr::mutate(page_lecture =
                        ifelse(comp_na_f(class_num, cal_entry$class_num),
                               slide_url, page_lecture))
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
              file.path(slide_dir, slide_class_dir, slide))) %>%
            dplyr::arrange(desc(date))
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
                          ifelse(comp_na_f(class_num, cal_entry$class_num),
                                 slide_url, page_lecture))
      } else {
        message("No slides found for class ", class_num, " on ",
                as.character(d))
      }
    }
  }
}

build_reading_assignment <- function(date, cal_entry, class_num, semester,
                                     metadata,
                                     md_extensions = get_md_extensions()) {
  date <- lubridate::as_date(date)
  if (! is.na(cal_entry$id_class) &&
      cal_entry$id_class %in% semester$rd_items$cal_id) {
    message("Making reading page for class #", cal_entry$class_num,
            " on ", as.character(date))
    rd_fname <- sprintf("reading_%02d.Rmd", cal_entry$class_num)
    rd_path <- rd_fname %>% file.path(root_dir, "content", "reading", .)
    rd_url <- rd_fname %>% stringr::str_replace("\\.Rmd$", "") %>%
      file.path("/reading", .)
    rd_page <- make_reading_page(cal_entry$id_class, semester,
                                 md_extensions = md_extensions)
    cat(rd_page, file = rd_path)
    schedule <- schedule %>%
      dplyr::mutate(page_reading =
                      ifelse(comp_na_f(class_num, cal_entry$class_num),
                             rd_url, page_reading))
  }
}

build_hw_assignment <- function(date, cal_entry, semester, metadata,
                                md_extensions = get_md_extensions()) {
  if (! is.na(cal_entry$id_hw)) {
    message("Making homework page for ", cal_entry$key_hw)
    links <- generate_hw_assignment(cal_entry$key_hw, semester, TRUE,
                                    md_extensions)
    schedule <- schedule %>%
      dplyr::mutate(page_hw = ifelse(comp_na_f(id_hw, cal_entry$id_hw),
                                     links['url'], page_hw))
  }
}
build_lab_assignment <- function(date, cal_entry, semester, metadata,
                                 md_extensions = get_md_extensions()) {

  if (!is.na(cal_entry$id_lab)) {
    message("Making lab page for lab ", cal_entry$key_lab )
    links <- generate_lab_assignment(cal_entry$key_lab, semester, TRUE,
                                     md_extensions)
    schedule <- schedule %>%
      dplyr::mutate(page_lab = ifelse(comp_na_f(id_lab, cal_entry$id_lab),
                                      links['url'], page_lab))
  }
}

build_assignments <- function(schedule, semester, metadata) {
  dates <- schedule$date

  has_labs <- "id_lab" %in% names(schedule)
  has_hw <- "id_homework" %in% names(schedule)
  root_dir <- metadata$root_dir
  slide_dir <- metadata$slide_dir


  for (d in dates) {
    d = lubridate::as_date(d)
    cal_entry <- schedule %>% dplyr::filter(date == d)
    dbg_checkpoint(g_cal_entry, cal_entry)

    assertthat::assert_that(nrow(cal_entry) == 1,
                            msg = stringr::str_c("Multiple calendar entries for date ",
                                                 as.character(lubridate::as_date(d)), "."))

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

    copy_slides(date, cal_entry, semester, metadata)
    build_reading_assignment(d, cal_entry, semester, metadata, md_extensions)
    build_hw_assignment(d, cal_entry, semester, metadata, md_extensions)
    build_lab_assignment(d, cal_entry, semester, metadata, md_extensions)

  }
}

generate_assignments <- function(semester, md_extensions = get_md_extensions()) {
  metadata <- semester$metadata

  schedule <- init_schedule(semester)
  tmp <- schedule_strip_finals(schedule, semester, metadata)
  schedule <- tmp$schedule
  final_exms <- tmp$final_exams

  tmp <- schedule %>% schedule_add_homework(semester, metadata)
  schedule <- tmp$schedule

  tmp <- schedule %>% schedule_widen(semester, metadata, TRUE)
  schedule <- tmp$schedule

  set_schedule_globals(schedule, semester, metadata)

  build_assignments(schedule, semester, metadata)

  lesson_plan <- schedule %>%
    # dplyr::filter(! event_id %in% c("FINAL_EXAM", "ALT_FINAL_EXAM")) %>%
    dplyr::select(date, title = topic, reading = page_reading,
                  assignment = page_homework, lecture = page_lecture,
                  lab = page_lab, topic) %>%
    dplyr::arrange(date) %>% dplyr::mutate(date = as.character(date)) %>%
    rowwise() %>% do(lessons = as.list(.)) %>%
    map(~map(.x, ~discard(.x, is.na))) %>%
    yaml::as.yaml() %>% expand_codes(context, semester)

  cat(lesson_plan, file = file.path(root_dir, "date", "lessons.yml")

  invisible(list(lesson_plan = lesson_plan, schedule = schedule))
}

regenerate_assignments <- function() {
  load_semester_db()
  generate_assignments()
}
