format_textbook_reading_item <- function(reading_item) {
  reading_item <- as.list(reading_item)
  output <- reading_item$markdown_title
  if (! is.na(reading_item$chapter)) {
    output <- stringr::str_c(output, ", ", reading_item$chapter)
  }
  if (! is.na(reading_item$pages)) {
    output <- stringr::str_c(output, ", ", reading_item$pages)
  }
  output <- output %>% stringr::str_trim() %>% add_period()
  output
}

format_textbook_reading <- function(reading_list) {
  # g_reading_list <<- reading_list
  # Nice trick for row-wise function calls thanks to
  # Jenny Bryan.
  # See https://speakerdeck.com/jennybc/row-oriented-workflows-in-r-with-the-tidyverse?slide=40
  if (nrow(reading_list) > 0) {
    output <- reading_list %>%
      purrr::transpose(.) %>%
      purrr::map_chr(format_textbook_reading_item)
  }  else {
    output <- NULL
  }
  output
}

format_handout_reading_item <- function(reading_item, online_location = getOption("semestr.online_reading_loc")) {
  reading_item <- as.list(reading_item)
  if(is.null(reading_item$url) || is.na(reading_item$url)) {
    pre = ""
    post = ""
    loc = stringr::str_c(" (", online_location, ")")
  } else {
    pre = "["
    post = stringr::str_c("](", reading_item$url, '){target="_blank"}')
    loc = ""
  }
  output <- stringr::str_c("Handout: ", pre, reading_item$citation, post)
  if (! is.na(reading_item$chapter)) {
    output <- stringr::str_c(output, ", ", reading_item$chapter)
  }
  if (! is.na(reading_item$pages)) {
    output <- stringr::str_c(output, ", ", reading_item$pages)
  }
  output <- output %>% stringr::str_trim() %>%
    stringr::str_c(loc) %>%
    add_period()
  output
}

format_handout_reading <- function(reading_list) {
  if (nrow(reading_list) > 0) {
    output <- reading_list %>% purrr::transpose() %>%
      purrr::map_chr(format_handout_reading_item)
  } else {
    output <- NULL
  }
  output
}

make_reading_assignment <- function(reading_entry) {
  textbook_reading <- reading_entry %>%
    dplyr::filter(textbook &
                    ! (optional | undergraduate_only | graduate_only ))
  handout_reading <- reading_entry %>%
    dplyr::filter(handout &
                    ! (optional | undergraduate_only | graduate_only ))
  ugrad_textbook_reading <- reading_entry %>%
    dplyr::filter(textbook & undergraduate_only )
  ugrad_handout_reading <- reading_entry %>%
    dplyr::filter(handout & undergraduate_only )
  grad_textbook_reading <- reading_entry %>%
    dplyr::filter(textbook & graduate_only )
  grad_handout_reading <- reading_entry %>%
    dplyr::filter(handout & graduate_only )
  optional_textbook_reading <- reading_entry %>% dplyr::filter(textbook & optional)
  optional_handout_reading <- reading_entry %>% dplyr::filter(handout & optional)

  reading_notes <- reading_entry %>% dplyr::filter(!is.na(reading_notes))

  has_req_reading <- (nrow(textbook_reading) + nrow(handout_reading)) > 0
  has_ugrad_reading <- (nrow(ugrad_textbook_reading) +
                          nrow(ugrad_handout_reading)) > 0
  has_grad_reading <- (nrow(grad_textbook_reading) +
                         nrow(grad_handout_reading)) > 0
  has_opt_reading <- (nrow(optional_textbook_reading) +
                        nrow(optional_handout_reading)) > 0
  has_any_reading <- has_req_reading || has_ugrad_reading ||
    has_grad_reading || has_opt_reading

  has_notes <- nrow(reading_notes) > 0

  output <- "## Reading:"
  if (! has_any_reading) {
    output <- stringr::str_c(stringr::str_trim(output), "",
                    "No new reading for today.",
                    "", sep = "\n")
  } else {
    if (has_req_reading) {
      readings <- c(format_textbook_reading(textbook_reading),
                    format_handout_reading(handout_reading)) %>%
        itemize()
      output <- stringr::str_c(stringr::str_trim(output),
                      "",
                      "### Required Reading (everyone):",
                      append_newline_if_needed(readings),
                      "",
                      "", sep = "\n")
    }
    if (has_ugrad_reading) {
      ug_readings <- c(format_textbook_reading(ugrad_textbook_reading),
                       format_handout_reading(ugrad_handout_reading)) %>%
        itemize()
      output <- stringr::str_c(stringr::str_trim(output),
                      "",
                      "### Required for Undergrads (optional for grad students):",
                      append_newline_if_needed(ug_readings),
                      "",
                      "", sep = "\n")
    }
    if (has_grad_reading) {
      g_readings <- c(format_textbook_reading(grad_textbook_reading),
                      format_handout_reading(grad_handout_reading)) %>%
        itemize()
      output <- stringr::str_c(stringr::str_trim(output),
                      "",
                      "### Required for Grad Students (optional for undergrads):",
                      append_newline_if_needed(g_readings),
                      "",
                      "", sep = "\n")
    }
    if (has_opt_reading) {
      extra_readings <- c(format_textbook_reading(optional_textbook_reading),
                          format_handout_reading(optional_handout_reading)) %>%
        itemize()
      output <- stringr::str_c(stringr::str_trim(output), "",
                      "### Optional Extra Reading:", "",
                      append_newline_if_needed(extra_readings), "",
                      sep = "\n")
    }
  }
  if (has_notes) {
    reading_note_str <- reading_notes$reading_notes %>% stringr::str_trim() %>%
      stringr::str_c(collapse = "\n\n")
    output <- stringr::str_c(stringr::str_trim(output), "",
                    ifelse(has_req_reading || has_opt_reading,
                           "### Reading Notes:",
                           "### Notes:"),
                    "", reading_note_str, "",
                    sep = "\n")
  }
  output
}

make_reading_page <- function(cal_id, semester) {
  cal_id <- enquo(cal_id)
  readings <- semester$rd_items %>% dplyr::filter(cal_id == !!cal_id) %>%
    merge_dates(semester) %>%
    left_join( dplyr::select(semester$class_topics, topic, rd_key),
               by = "rd_key")
  rd_date <- unique(readings$date)
  assertthat::assert_that(length(rd_date) == 1,
                          msg = "A calendar ID should have a unique date (make_reading)")
  rd_topic <- unique(readings$topic)
  class_num <- cal_entry$class
  this_class_num <<- class_num
  this_class_date <<- rd_date

  delim <- "---"
  header <- tibble(title = rd_topic,
                   class_date = as.character(rd_date),
                   class_number = class_num, weight = class_num,
                   slug = sprintf("reading_%02d", class_num),
                   pubdate = as.character(pub_date),
                   date = as.character(rd_date),
                   output = list("blogdown::html_page" =
                                   list(md_extensions = md_extensions))
  ) %>%
    as.yaml() %>% str_trim("right") %>%
    str_c(delim, ., delim, sep = "\n")
  rd_page <- str_c(
    header,
    make_short_hw_assignment(cal_entry) %>% escape_dollar(),
    make_reading_assignment(reading) %>% escape_dollar(),
    sep = "\n"
  ) %>% expand_codes()

  rd_page
}

