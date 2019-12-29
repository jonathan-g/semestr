generate_assignments <- function(semester, md_extensions = get_md_extensions()) {
  metadata <- semester$metadata

  schedule <- semester$calendar %>%
    dplyr::filter(cal_type %in% c("class", "exam", "homework", "lab", "holiday"),
                  ! topic_key %in%
                    add_key_prefix(c("FINAL_EXAM", "ALT_FINAL_EXAM"),
                                   semester$metadata, "exam")) %>%
    dplyr::select(id = cal_id, date, key = topic_key, cal_type) %>%
    dplyr::mutate(page = NA_character_,
                  # dates might be datetimes, so convert everything to calendar
                  # dates.
                  date = lubridate::as_date(date, tz = metadata$tz),
                  cal_type = type2idx(cal_type, metadata))

  hw_due <- semester$due_dates %>%
    dplyr::filter(type == "homework", action %in% c("homework", "report")) %>%
    dplyr::filter(cal_id %in% semester$calendar$cal_id)

  hw <- semester$hw_asgt %>% dplyr::filter(due_key %in% hw_due$due_key) %>%
    add_key_prefix(metadata, "homework", "hw_key")

  missing_hw <- hw %>%
    dplyr::filter(! (hw_key %in% schedule$key & cal_id %in% schedule$id))

  missing_hw_entries <- missing_hw %>%
    dplyr::select( key = hw_key, id = due_cal_id) %>%
    dplyr::left_join(dplyr::select(semester$calendar, id = cal_id, date),
              by = c("id")) %>%
    dplyr::mutate(cal_type = "homework",
                  date = as_date(date, tz = metadata$tz))

  schedule <- schedule %>% dplyr::bind_rows(missing_hw_entries)

  sched_check <- schedule %>% dplyr::group_by(date, cal_type) %>%
    dplyr::summarize(count = n()) %>% dplyr::ungroup() %>%
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

  schedule <- schedule %>% mutate(cal_type = type2col(cal_type, metadata)) %>%
    tidyr::pivot_wider(names_from = cal_type, values_from = c(id, key,
                                                              page)) %>%
    dplyr::left_join( dplyr::select(class_topics, key_class = topic_key,
                                    topic), by = "key_class") %>%
    dplyr::left_join( dplyr::select(semester$calendar, id_class = cal_id,
                                    class_num), by = "id_class" )

  for (col in metadata$type2col) {
    key_col <- stringr::str_c("key_", col)
    if (key_col %in% names(schedule)) {
      q_key_col <- enquo(key_col)
      schedule <- strip_key_prefix(schedule, metadata, col2type(col, metadata),
                                   !!q_key_col)
    }
  }

  schedule <- schedule %>% dplyr::rename(page_reading = page_class)

  dates <- schedule$date

  has_labs <- "id_lab" %in% names(schedule)
  has_hw <- "id_homework" %in% names(schedule)
  root_dir <- metadata$root_dir
  slide_dir <- metadata$slide_dir

  for (d in dates) {
    cal_entry <- schedule %>% dplyr::filter(date == d)
    dbg_checkpoint(g_cal_entry, cal_entry)

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

    if (! is.na(class_num)) {
      slide_class_dir <- sprintf("class_%02d", class_num)
      slide_url <- file.path("/slides", slide_class_dir, fsep = "/")

      if (file.exists(file.path(slide_dir, slide_class_dir,
                                       "index.html"))) {
        message("HTML slide_url for class ", class_num, " on ", d, " is ",
                slide_url)
        semester <- semester %>%
          dplyr::mutate(lecture_page = ifelse(class == class_num,
                                              slide_url, lecture_page))
      } else {
        slides <- list.files(file.path(slide_dir, slide_class_dir),
                             pattern = "*.ppt*")
        if (length(slides) > 0) {
          if (length(slides) == 1) {
            these_slides <- slides[1]
            message("One ppt slide found for class ", class_num, " on ", d,
                    ": ", these_slides)
          } else {
            slide_df <- tibble::tibble(slide = slides) %>%
              dplyr::mutate(date = file.mtime(file.path(slide_dir, slide_class_dir, slide))) %>%
              dplyr::arrange(desc(date))
            these_slides <- slide_df$slide[1]
            message(length(slides), " ppt slides found for class ", class_num,
                    " on ", d, ". Choosing ", these_slides)
          }
          slide_url <- file.path(slide_url, these_slides, fsep = "/") %>%
            URLencode()
          message("slide_url = ", slide_url)
          semester <- semester %>%
            dplyr::mutate(lecture_page = ifelse(class == class_num,
                                                slide_url, lecture_page))
        } else {
          message("No slides found for class ", class_num, " on ", d)
        }
      }

      if (! is.na(cal_entry$id_class) &&
          cal_entry$id_class %in% semester$rd_items$cal_id) {
        message("Making reading page for class #", cal_entry$class_num,
                " on ", d)
        rd_fname <- sprintf("reading_%02d.Rmd", cal_entry$class_num)
        rd_path <- rd_fname %>% file.path(root_dir, "content", "reading", .)
        rd_url <- rd_fname %>% str_replace("\\.Rmd$", "") %>%
          file.path("/reading", .)
        rd_page <- make_reading_page(cal_entry$id_class, semester,
                                     md_extensions = md_extensions)
        cat(rd_page, file = rd_path)
        schedule <- schedule %>%
          dplyr::mutate(page_reading = ifelse(class_num == cal_entry$class_num,
                                              rd_url, page_reading))
      }
    }

    if (! is.na(cal_entry$id_hw)) {
      hw <- semester$hw_asgt %>% filter(cal_id == cal_entry$id_hw)
      hw_slug <- make_hw_slug(hw)
      hw_fname <- str_c(hw_slug, ".Rmd")
      message("Making homework page for assignment ",
              ifelse(is.na(hw$hw_num), hw$hw_key,
                           stringr::str_c("# ", hw$hw_num)),
              " (index = ", hw$hw_id,
              ", slug = ", hw_slug, ", filename = ", hw_fname, ")")
      hw_path <- hw_fname %>% file.path(root_dir, "content", "assignment", .)
      hw_url <- hw_fname %>% str_replace("\\.Rmd$", "")
      hw_page <- make_hw_page(cal_entry$key_hw, semester, TRUE,
                              md_extensions = md_extensions)
      cat(hw_page, file = hw_path)
      schedule <- schedule %>%
        dplyr::mutate(homework_page = ifelse(id_hw == cal_entry$id_hw,
                                             hw_url, hw_page))
    }

    if (cal_entry$has_lab) {
      lab <- semester$lab_asgt %>% filter(cal_id == cal_entry$id_lab)
      message("Making lab page for lab #", lab$lab_num )
      lab_assignment <-
        make_lab_assignment(lab$lab_key, semester = semester,
                            md_extensions = md_extensions)
      lab_path <- lab_assignment['path']
      lab_url <- lab_assignment['url']
      semester <- semester %>%
        dplyr::mutate(lab_page = ifelse(lab_num == cal_entry$lab_num,
                                        lab_url, lab_page))
    }
  }

  semester %>%
    # dplyr::filter(! event_id %in% c("FINAL_EXAM", "ALT_FINAL_EXAM")) %>%
    dplyr::select(date, title = topic, reading = reading_page,
                  assignment = homework_page, lecture = lecture_page, lab = lab_page, topic) %>%
    dplyr::arrange(date) %>% dplyr::mutate(date = as.character(date)) %>%
    rowwise() %>% do(lessons = as.list(.)) %>%
    map(~map(.x, ~discard(.x, is.na))) %>%
    yaml::as.yaml() %>% expand_codes() %T>%
    cat(file = file.path(root_dir, "data", "lessons.yml")) -> lesson_plan

  invisible(list(lesson_plan = lesson_plan, semester = semester))
}

regenerate_assignments <- function() {
  load_semester_db()
  generate_assignments()
}
