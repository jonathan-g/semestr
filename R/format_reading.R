#' Format a Reading Item
#'
#' Takes a reading item and formats it.
#'
#' @param reading_item The reading item (a row from the semester  `rd_items`
#'   data frame)
#'
#' @return A character string with the formatted output.
#'
#' @name format_reading_item
NULL

#' Format a Collection of Reading Items
#'
#' Takes a collection of reading items and formats them.
#'
#' @param reading_list A data frame with rows corresponding to reading items
#'   (a subset from the semester `rd_items` data frame)
#'
#' @return A character vector with the formatted output for each item.
#'
#' @name format_reading_items
NULL

#' @describeIn format_reading_item Format reading from a textbook.
#'
#' @export
format_textbook_reading_item <- function(reading_item) {
  reading_item <- as.list(reading_item)
  output <- reading_item$markdown_title
  if (! is_mt_or_na(reading_item$chapter)) {
    output <- stringr::str_c(output, ", ", reading_item$chapter)
  }
  if (! is_mt_or_na(reading_item$pages)) {
    output <- stringr::str_c(output, ", ", reading_item$pages)
  }
  output <- output %>% stringr::str_trim() %>% add_period()
  output
}


#' @describeIn format_reading_items Format a collection of textbook readings.
#'
#' @export
format_textbook_reading <- function(reading_list) {
  # Nice trick for row-wise function calls thanks to
  # Jenny Bryan.
  # See https://speakerdeck.com/jennybc/row-oriented-workflows-in-r-with-the-tidyverse?slide=40
  if (nrow(reading_list) > 0) {
    output <- reading_list %>%
      purrr::pmap(list) %>%
      purrr::map_chr(format_textbook_reading_item)
  }  else {
    output <- NULL
  }
  output
}


#' @describeIn format_reading_item Format a handout reading item.
#'
#' @param online_location An URL for where to find the handout.
#'
#' @export
format_handout_reading_item <- function(reading_item, online_location = getOption("semestr.online_reading_loc")) {
  reading_item <- as.list(reading_item)
  if(is_mt_or_na(reading_item$url) || is.null(reading_item$url)) {
    pre = ""
    post = ""
    loc = stringr::str_c(" (", online_location, ")")
  } else {
    pre = "["
    post = stringr::str_c("](", reading_item$url, '){target="_blank"}')
    loc = ""
  }
  output <- stringr::str_c("Handout: ", pre, reading_item$citation, post)
  if (! is_mt_or_na(reading_item$chapter)) {
    output <- stringr::str_c(output, ", ", reading_item$chapter)
  }
  if (! is_mt_or_na(reading_item$pages)) {
    output <- stringr::str_c(output, ", ", reading_item$pages)
  }
  output <- output %>% stringr::str_trim() %>%
    stringr::str_c(loc) %>%
    add_period()
  output
}


#' @describeIn format_reading_items Format reading from multiple handouts.
#'
#' @export
format_handout_reading <- function(reading_list) {
  if (nrow(reading_list) > 0) {
    output <- reading_list %>% purrr::pmap(list) %>%
      purrr::map_chr(format_handout_reading_item)
  } else {
    output <- NULL
  }
  output
}

#' @describeIn format_reading_item Format a web page reading item.
#'
#' @param online_location An URL for where to find the web page.
#'
#' @export
format_web_reading_item <- function(reading_item, online_location = getOption("semestr.online_reading_loc")) {
  reading_item <- as.list(reading_item)
  if(is_mt_or_na(reading_item$url) || is.null(reading_item$url)) {
    pre = ""
    post = ""
    loc = stringr::str_c(" (", online_location, ")")
  } else {
    pre = "["
    post = stringr::str_c("](", reading_item$url, '){target="_blank"}')
    loc = ""
  }
  output <- stringr::str_c("Web Page: ", pre, reading_item$citation, post)
  if (! is_mt_or_na(reading_item$chapter)) {
    output <- stringr::str_c(output, ", ", reading_item$chapter)
  }
  if (! is_mt_or_na(reading_item$pages)) {
    output <- stringr::str_c(output, ", ", reading_item$pages)
  }
  output <- output %>% stringr::str_trim() %>%
    stringr::str_c(loc) %>%
    add_period()
  output
}


#' @describeIn format_reading_items Format reading from multiple web pages.
#'
#' @export
format_web_reading <- function(reading_list) {
  if (nrow(reading_list) > 0) {
    output <- reading_list %>% purrr::pmap(list) %>%
      purrr::map_chr(format_web_reading_item)
  } else {
    output <- NULL
  }
  output
}

#' @describeIn format_reading_item Format a YouTube video.
#'
#' @param online_location An URL for where to find the video.
#'
#' @export
format_youtube_reading_item <- function(reading_item, online_location = getOption("semestr.online_reading_loc")) {
  reading_item <- as.list(reading_item)
  if(is_mt_or_na(reading_item$url) || is.null(reading_item$url)) {
    pre = ""
    post = ""
    loc = stringr::str_c(" (", online_location, ")")
  } else {
    pre = "["
    post = stringr::str_c("](", reading_item$url, '){target="_blank"}')
    loc = ""
  }
  output <- stringr::str_c("YouTube Video: ", pre, reading_item$citation, post)
  if (! is_mt_or_na(reading_item$chapter)) {
    output <- stringr::str_c(output, ", ", reading_item$chapter)
  }
  if (! is_mt_or_na(reading_item$pages)) {
    output <- stringr::str_c(output, ", ", reading_item$pages)
  }
  output <- output %>% stringr::str_trim() %>%
    stringr::str_c(loc) %>%
    add_period()
  output
}


#' @describeIn format_reading_items Format reading from multiple YouTube videos.
#'
#' @export
format_youtube_reading <- function(reading_list) {
  if (nrow(reading_list) > 0) {
    output <- reading_list %>% purrr::pmap(list) %>%
      purrr::map_chr(format_youtube_reading_item)
  } else {
    output <- NULL
  }
  output
}

#' Make a Reading Assignment
#'
#' Format all readings from a reading assignment entry.
#'
#' @param reading_entry A reading entry assembled from the semester database
#'   by `\link{make_reading_page}`
#'
#' @return A formatted reading assignment, suitable for including in an
#'   assignment page.
#'
#' @keywords internal
make_reading_assignment <- function(reading_entry) {
  reading_entry <- reading_entry %>%
    dplyr::arrange(dplyr::desc(.data$rd_prologue), .data$rd_epilogue,
                   .data$rd_item_id)
  textbook_reading <- reading_entry %>%
    dplyr::filter(.data$textbook,
                  ! (.data$optional | .data$undergraduate_only |
                       .data$graduate_only ))
  handout_reading <- reading_entry %>%
    dplyr::filter(.data$handout,
                  ! (.data$optional | .data$undergraduate_only |
                       .data$graduate_only ))
  web_reading <- reading_entry %>%
    dplyr::filter(.data$web_page,
                  ! (.data$optional | .data$undergraduate_only |
                       .data$graduate_only ))
  youtube_videos <- reading_entry %>%
    dplyr::filter(.data$youtube,
                  ! (.data$optional | .data$undergraduate_only |
                       .data$graduate_only ))
  ugrad_textbook_reading <- reading_entry %>%
    dplyr::filter(.data$textbook, .data$undergraduate_only )
  ugrad_handout_reading <- reading_entry %>%
    dplyr::filter(.data$handout, .data$undergraduate_only )
  ugrad_web_reading <- reading_entry %>%
    dplyr::filter(.data$web_page, .data$undergraduate_only )
  ugrad_youtube_videos <- reading_entry %>%
    dplyr::filter(.data$youtube, .data$undergraduate_only )
  grad_textbook_reading <- reading_entry %>%
    dplyr::filter(.data$textbook, .data$graduate_only )
  grad_handout_reading <- reading_entry %>%
    dplyr::filter(.data$handout, .data$graduate_only )
  grad_web_reading <- reading_entry %>%
    dplyr::filter(.data$web_page, .data$graduate_only )
  grad_youtube_videos <- reading_entry %>%
    dplyr::filter(.data$youtube, .data$graduate_only )
  optional_textbook_reading <- reading_entry %>%
    dplyr::filter(.data$textbook, .data$optional)
  optional_handout_reading <- reading_entry %>%
    dplyr::filter(.data$handout, .data$optional)
  optional_web_reading <- reading_entry %>%
    dplyr::filter(.data$web_page, .data$optional)
  optional_youtube_videos <- reading_entry %>%
    dplyr::filter(.data$youtube, .data$optional)

  reading_notes <- reading_entry %>%
    dplyr::filter(!is.na(.data$reading_notes))

  youtube_items <- reading_entry %>%
    dplyr::filter(.data$youtube)

  has_req_reading <- (nrow(textbook_reading) + nrow(handout_reading) +
                        nrow(web_reading) + nrow(youtube_videos)) > 0
  has_ugrad_reading <- (nrow(ugrad_textbook_reading) +
                          nrow(ugrad_handout_reading) +
                          nrow(ugrad_web_reading) +
                          nrow(ugrad_youtube_videos)) > 0
  has_grad_reading <- (nrow(grad_textbook_reading) +
                         nrow(grad_handout_reading) +
                         nrow(grad_web_reading) +
                         nrow(grad_youtube_videos)) > 0
  has_opt_reading <- (nrow(optional_textbook_reading) +
                        nrow(optional_handout_reading) +
                        nrow(optional_web_reading) +
                        nrow(optional_youtube_videos)) > 0
  has_any_reading <- has_req_reading || has_ugrad_reading ||
    has_grad_reading || has_opt_reading
  has_any_youtube_videos <- nrow(youtube_videos) > 0

  has_notes <- nrow(reading_notes) > 0

  output <- "## Reading:"
  if (! has_any_reading) {
    output <- stringr::str_c(stringr::str_trim(output), "",
                             "No new reading for today.",
                             "", sep = "\n")
  } else {
    if (has_req_reading) {
      readings <- c(format_textbook_reading(textbook_reading),
                    format_handout_reading(handout_reading),
                    format_web_reading(web_reading),
                    format_youtube_reading(youtube_videos)) %>%
        itemize()
      output <- stringr::str_c(stringr::str_trim(output),
                               "",
                               "### Required Reading (everyone):",
                               append_newline_if_needed(readings, TRUE, 1),
                               sep = "\n")
    }
    if (has_ugrad_reading) {
      ug_readings <- c(format_textbook_reading(ugrad_textbook_reading),
                       format_handout_reading(ugrad_handout_reading),
                       format_web_reading(ugrad_web_reading),
                       format_youtube_reading(ugrad_youtube_videos)) %>%
        itemize()
      output <- stringr::str_c(stringr::str_trim(output),
                               "",
                               "### Required for Undergrads (optional for grad students):",
                               append_newline_if_needed(ug_readings, TRUE, 1),
                               sep = "\n")
    }
    if (has_grad_reading) {
      g_readings <- c(format_textbook_reading(grad_textbook_reading),
                      format_handout_reading(grad_handout_reading),
                      format_web_reading(grad_web_reading),
                      format_youtube_reading(grad_youtube_videos)) %>%
        itemize()
      output <- stringr::str_c(stringr::str_trim(output),
                               "",
                               "### Required for Grad Students (optional for undergrads):",
                               append_newline_if_needed(g_readings, TRUE, 1),
                               sep = "\n")
    }
    if (has_opt_reading) {
      extra_readings <- c(format_textbook_reading(optional_textbook_reading),
                          format_handout_reading(optional_handout_reading),
                          format_web_reading(optional_web_reading),
                          format_youtube_reading(optional_youtube_videos)) %>%
        itemize()
      output <- stringr::str_c(stringr::str_trim(output), "",
                               "### Optional Extra Reading:",
                               append_newline_if_needed(extra_readings, TRUE, 1),
                               sep = "\n")
    }
  }
  if (has_notes) {
    reading_note_str <- reading_notes %>%
      dplyr::arrange(dplyr::desc(.data$rd_prologue), .data$rd_epilogue,
                     .data$rd_item_id) %>%
      dplyr::pull("reading_notes") %>%
      stringr::str_trim(.) %>%
      stringr::str_c(collapse = "\n\n")
    output <- stringr::str_c(stringr::str_trim(output), "",
                             ifelse(has_req_reading || has_opt_reading,
                                    "### Reading Notes:",
                                    "### Notes:"),
                             "", reading_note_str, "",
                             sep = "\n")
  }
  if (has_any_youtube_videos) {
    youtube_videos_str <- youtube_items %>%
      dplyr::mutate(item_str =
                      stringr::str_c('`r htmltools::HTML(\'{{< youtube id="',
                                     .data$youtube_id, '" title="',
                                     .data$short_markdown_title,
                                     '" >}}\')`')) %>%
      dplyr::pull("item_str") %>% itemize()
    youtube_title <- "### YouTube Video"
    if (nrow(youtube_items) > 1) {
      youtube_title <- stringr::str_c(youtube_title, "s")
    }
    output <- stringr::str_c(stringr::str_trim(output), "",
                             youtube_title, "", youtube_videos_str, "",
                             sep = "\n")
  }
  output
}

#' Make a Reading Assignment Page
#'
#' Make a page for the reading assignment for one class, from the semester
#'   database.
#'
#' @param cal_id The calendar ID for this assignment.
#' @param semester A semester object (a list returned by
#'   `\link{load_semester_db}`).
#' @param use_pdfs Add a `pdf_url` field to the YAML header so a PDF file will
#'   be generated for the reading assignment.
#'
#' @return A character string with the Markdown content for the reading
#'   assignment page.
#'
#' @export
make_reading_page <- function(cal_id, semester, use_pdfs = TRUE){
  cal_id <- enquo(cal_id)
  reading <- semester$rd_items %>%
    dplyr::filter(.data$cal_id == !!cal_id) %>%
    # merge_dates(semester) %>%
    dplyr::left_join(dplyr::select(semester$calendar, "cal_id", "class_num",
                                   "week_num"),
                     by = "cal_id") %>%
    dplyr::left_join( dplyr::select(semester$class_topics, "topic",
                                    "rd_grp_key"),
                      by = "rd_grp_key")
  rd_date <- unique(reading$date)
  assertthat::assert_that(length(rd_date) == 1,
                          msg = "A calendar ID should have a unique date (make_reading)")
  rd_topic <- unique(reading$topic)
  assertthat::assert_that(length(rd_date) == 1,
                          msg = "A calendar ID should have a unique topic (make_reading)")
  class_num <- unique(reading$class_num)
  class_key <- unique(reading$class_key)
  assertthat::assert_that(length(class_num) == 1,
                          msg = "A calendar ID should have a unique class # (make_reading)")
  key <- unique(reading$rd_grp_key)
  assertthat::assert_that(length(key) == 1,
                          msg = "A calendar ID should have a unique reading key # (make_reading)")
  if (semester$has_notices) {
    notices <- semester$notices %>%
      dplyr::filter(.data$topic_id == class_key, ! is.na(.data$notice))
  } else {
    notices <- NULL
  }

  homework <- semester$hw_asgt %>%
    dplyr::filter(.data$cal_id == !!cal_id) %>%
    # merge_dates(semester) %>%
    dplyr::left_join( dplyr::select(semester$hw_items, -"hw_num", -"cal_id"),
                      by = "hw_grp_key")
  delim <- "---"
  header <- tibble::tibble(title = rd_topic,
                           class_date = lubridate::as_date(rd_date) %>% as.character(),
                           class_number = class_num, weight = class_num,
                           slug = sprintf("reading_%02d", class_num),
                           pubdate = as.character(semester$semester_dates$pub_date),
                           date = lubridate::as_date(rd_date) %>% as.character(),
                           output = list("blogdown::html_page" =
                                           list(md_extensions = get_md_extensions()))
  )
  if (use_pdfs) {
    header <- header %>%
      dplyr::mutate(pdf_url = stringr::str_c("/files/reading_asgts/",
                                             .data$slug, ".pdf"))
  }
  header <- header %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  rd_page <- stringr::str_c(
    header,
    make_notice(notices),
    # make_short_hw_assignment(homework) %>% escape_dollar(),
    make_reading_assignment(reading) %>% escape_dollar(),
    sep = "\n"
  )
  asgt <- reading %>%
    dplyr::select("cal_id", "rd_grp_key", "cal_key", "date", "topic",
                  "class_num") %>%
    dplyr::distinct()
  assertthat::assert_that(nrow(asgt) == 1,
                          msg = "A calendar ID should have a consistent reading assignment (make_reading)")
  context <- make_context(asgt, "reading", semester)

  rd_page <- rd_page %>%
    expand_codes(context, semester, params = list(this_class_num = class_num,
                                                  this_class_date = rd_date))
  rd_page
}

