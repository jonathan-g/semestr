get_hw_assignment <- function(key, semester) {
  assignment <- semester$hw_asgt %>% dplyr::filter(.data$hw_grp_key == key)
  assertthat::assert_that(nrow(assignment) == 1,
                          msg = stringr::str_c(
                            "There should only be one homework assignment for a given key: ",
                            "key ", key, " has ", nrow(assignment), " assignments.")
  )

  assignment <- as.list(assignment)
  assignment
}

make_hw_slug <- function(hw_asgt) {
  message("Making HW slug for ", hw_asgt$hw_grp_key,
          ", is_numbered = ", hw_asgt$hw_is_numbered,
          ", hw_num = ", hw_asgt$hw_num)
  if (hw_asgt$hw_is_numbered) {
    slug <- sprintf("homework_%02d", hw_asgt$hw_num)
  } else {
    slug <- hw_asgt$hw_slug
  }
  slug
}

make_hw_solution_page <- function(solution, semester, slug = NA_character_) {
  if (is_mt_or_na(slug) || is.null(slug)) {
    slug = sprintf("homework_%02d", solution$hw_num)
  }

  message("Generating markdown for solutions to homework #", solution$hw_num,
          ", slug = ", slug)

  delim <- "---"
  header <- list(
    title = solution$hw_sol_title,
    hw_number = solution$hw_num,
    pubdate = as.character(solution$hw_sol_pub_date),
    date = as.character(solution$hw_due_date),
    pdf_url = solution$hw_sol_pdf_url,
    slug = stringr::str_c(slug, "_", solution$hw_sol_filename)) %>%
    purrr::discard(is_mt_or_na) %>%
    c(
      list(output = list("blogdown::html_page" =
                           list(md_extensions = get_md_extensions(),
                                toc = TRUE)))
    ) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>% #nolint
    stringr::str_c(delim, ., delim, sep = "\n")
  context <- make_context(solution, "homework solution", semester)
  hw_solution_page <- stringr::str_c(
    header,
    solution$sol_markdown,
    sep = "\n"
  ) %>% expand_codes(context, semester)
  hw_solution_page
}

make_hw_solution <- function(solution, assignment, semester, slug = NA_character_) {
  if (is_mt_or_na(slug)) {
    slug = sprintf("homework_%02d", assignment$hw_num)
  }
  fname <- stringr::str_c(slug, "_", solution$hw_sol_filename, ".Rmd")
  solution_path <- fname %>%
    file.path(semester$root_dir, "content", "homework_solutions/", .)
  solution_url <- fname %>% stringr::str_replace("\\.Rmd$", "") %>%
    file.path("/homework_solutions", .)
  message("Making solutions file for homework #", assignment$hw_num, ": ",
          solution_path)
  hw_solution_page <- make_hw_solution_page(solution, semester, slug)
  cat(hw_solution_page, file = solution_path)
  c(path = solution_path, url = solution_url)
}

make_hw_asgt_section_content <- function(items, heading, also_flag) {
  output <- NULL
  message("Making homework section ", heading, ": ")
  message("class(items) = [", stringr::str_c(class(items), collapse = ","), "]")
  message("  dim = (", stringr::str_c(dim(items), collapse = ","),
          ", length = ", length(items))
  if (nrow(items) > 0) {
    items <- items %>%
      dplyr::mutate(hw_self_assess = tidyr::replace_na(.data$hw_self_assess,
                                                       FALSE),
                    hw_optional = tidyr::replace_na(.data$hw_optional, FALSE)
      )
    self_study_items <- items %>% dplyr::filter(.data$hw_self_assess) %>%
      dplyr::pull("homework") %>% unique() %>% itemize()
    optional_items <- items %>% dplyr::filter(.data$hw_optional) %>%
      dplyr::pull("homework") %>% unique() %>% itemize()
    turn_in_items <- items %>% dplyr::filter(!.data$hw_self_assess,
                                             !.data$hw_optional) %>%
      dplyr::pull("homework") %>% unique() %>% itemize()
    item_output <- ""
    if (stringr::str_length(self_study_items) > 0) {
      item_output <- stringr::str_c(
        item_output,
        "**Self-study:** Work these exercises, but do not turn them in.",
        self_study_items, sep = "\n\n"
      )
    }
    if (stringr::str_length(turn_in_items) > 0) {
      if (stringr::str_length(self_study_items) > 0) {
        item_output <- stringr::str_c(
          item_output,
          "**Turn in:** Work these exercises and turn them in.",
          sep = "\n\n")
      }
      item_output <- stringr::str_c(item_output, turn_in_items, sep = "\n\n")
    }
    if (stringr::str_length(optional_items) > 0) {
      item_output <- stringr::str_c(
        item_output,
        "**Optional:** The following exercises are optional. You can turn them in for extra credit.",
        optional_items,
        sep = "\n\n")
    }
  }
  if (! is.null(heading)) {
    output <-
      stringr::str_c(output,
                     stringr::str_c("**", heading,
                                    ifelse(also_flag,
                                           ",** also do the following:",
                                           ":**")),
                     sep = "\n")
  }
  output <- stringr::str_c(output, item_output, sep = "\n\n")

  invisible(output)
}

make_hw_asgt_content <- function(key, semester, use_solutions = FALSE) {
  assignment <- get_hw_assignment(key, semester)

  message("Making content for HW ", key)

  items <- semester$hw_items %>% dplyr::filter(.data$hw_grp_key == key) %>%
    # merge_dates(semester) %>%
    dplyr::arrange(.data$hw_item_id)
  if (use_solutions && ! is.null(semester$hw_sol)) {
    solutions <- semester$hw_sol %>% dplyr::filter(.data$sol_grp_key == key)
    if (nrow(solutions) > 0) {
      solutions <- solutions %>%
        dplyr::mutate( due_cal_id = assignment$due_cal_id,
                       due_date = assignment$due_date) %>%
        # merge_dates(semester, id_col = "sol_pub_cal_id",
        #             date_col = "sol_pub_date") %>%
        dplyr::mutate(sol_pub_date =
                        lubridate::as_datetime(.data$sol_pub_date,
                                               tz = get_semestr_tz())) %>%
        dplyr::filter(.data$sol_pub_date <= lubridate::now()) %>%
        dplyr::arrange(.data$sol_id)
    } else {
      solutions <- NULL
    }
  } else {
    solutions <- NULL
  }

  hw <- items %>%
    dplyr::filter(! is.na(.data$homework),
                  stringr::str_length(.data$homework) > 0)
  hw_a <- hw %>% dplyr::filter(! .data$hw_prologue, !.data$hw_epilogue)
  grad_hw <- hw_a %>% dplyr::filter(.data$graduate_only)
  ugrad_hw <- hw_a %>% dplyr::filter(.data$undergraduate_only)
  everyone_hw <- hw_a %>% dplyr::filter(! .data$graduate_only,
                                        ! .data$undergraduate_only)

  prologue <- hw %>% dplyr::filter(.data$hw_prologue)
  epilogue <- hw %>% dplyr::filter(.data$hw_epilogue)

  notes <- hw %>% dplyr::filter(! is.na(.data$homework_notes))
  main_notes <- notes %>% dplyr::filter(! (.data$hw_prologue | .data$hw_epilogue))
  grad_notes <- main_notes %>% dplyr::filter(.data$graduate_only)
  ugrad_notes <- main_notes %>% dplyr::filter(.data$undergraduate_only)
  everyone_notes <- main_notes %>%
    dplyr::filter(!.data$graduate_only & !.data$undergraduate_only)
  prologue_notes <- notes %>% dplyr::filter(.data$hw_prologue)
  epilogue_notes <- notes %>% dplyr::filter(.data$hw_epilogue)

  message("Building content: ",
          nrow(prologue), " prologue items, ",
          nrow(epilogue), " epilogue items, ",
          nrow(notes), " notes", "\n  ",
          nrow(everyone_hw), " items for everyone, ",
          nrow(ugrad_hw), " items for undergrads, ",
          nrow(grad_hw), " items for grads."
          )

  output <- NULL

  if (! is.null(solutions) && nrow(solutions) >= 1) {
    message("Making homework solutions")
    output <- stringr::str_c(output, "## Solutions:\n\n")
    for (i in seq(nrow(solutions))) {
      this_sol <- solutions[i,]
      sol <- make_hw_solution(this_sol, assignment, semester)
      output <- output %>% stringr::str_c("* [", this_sol$hw_sol_title, "](",
                                          sol['url'], ")\n")
    }
    output <- stringr::str_c(output, "\n")
  }

  message("Starting content generation")

  output <- stringr::str_c(output, "## Homework", sep = "\n\n")
  if (nrow(prologue) > 0) {
    message("  Adding prologue")
    prologue_str <- stringr::str_c(
      purrr::discard(prologue$homework,
                     ~is_mt_or_na(.x) || .x == "") %>%
        unique(),
      collapse = "\n\n")
    prologue_str <- stringr::str_c("### Preliminary Information",
                                   prologue_str, sep = "\n\n")

  } else {
    prologue_str <- NULL
  }

  if (nrow(epilogue) > 0) {
    message("  Adding epiloque")
    epilogue_str <- stringr::str_c(
      purrr::discard(epilogue$homework,
                     ~is_mt_or_na(.x) || .x == "") %>%
        unique(),
      collapse = "\n\n")
    epilogue_str <- stringr::str_c("### General Notes:",
                                   epilogue_str, sep = "\n\n")
  } else {
    epilogue_str <-  NULL
  }

  output <- stringr::str_c(output, prologue_str, "", sep = "\n\n")
  if (nrow(ugrad_hw) > 0) {
    message("  Making undergrad content")
    ugrad_hw_items <- make_hw_asgt_section_content(
      ugrad_hw,"Undergraduate Students", nrow(everyone_hw) > 0
    )
  } else {
    ugrad_hw_items <- NULL
  }
  if (nrow(grad_hw) > 0) {
    message("  Making grad content")
    grad_hw_items <- make_hw_asgt_section_content(
      grad_hw, "Graduate Students", nrow(everyone_hw) > 0
    )
  } else {
    grad_hw_items <- NULL
  }
  if (nrow(everyone_hw) > 0) {
    message("  Making everyone content")
    if (nrow(ugrad_hw) + nrow(grad_hw) > 0) {
      sec_hdr = "Everyone"
    } else {
      sec_hdr = NULL
    }
    everyone_hw_items <- make_hw_asgt_section_content(
      everyone_hw, sec_hdr, FALSE)
  } else {
    everyone_hw_items <- NULL
  }
  if (all(is.null(grad_hw_items), is.null(ugrad_hw_items))) {
    message(" All content is for everyone")
    output <- stringr::str_c(stringr::str_trim(output), "",
                             "### Homework Exercises:", "",
                             everyone_hw_items,
                             "", sep = "\n")
  } else {
    message("  Combining undergrad, grad, and everyone content.")
    output <- stringr::str_c(stringr::str_trim(output), "",
                             "### Homework Exercises:", "",
                             itemize(c(everyone_hw_items, ugrad_hw_items,
                                       grad_hw_items)),
                             "", sep = "\n")
  }

  output <- stringr::str_c(stringr::str_trim(output), epilogue_str, "",
                           sep = "\n\n")

  message(" Making notes.")
  everyone_notes <- dplyr::bind_rows(prologue_notes, everyone_notes,
                                     epilogue_notes) %>%
    dplyr::distinct()

  if (nrow(everyone_notes) > 0) {
    message("  Making everyone notes")
    everyone_note_items <- everyone_notes$homework_notes %>%
      stringr::str_trim("right") %>% stringr::str_c(collapse = "\n\n")
  } else {
    everyone_note_items <- NULL
  }

  if (nrow(ugrad_notes) > 0) {
    message("  Making undergrad notes")
    ugrad_note_items <- ugrad_notes$homework_notes %>%
      stringr::str_trim("right") %>% stringr::str_c(collapse = "\n\n")
  } else {
    ugrad_note_items <- NULL
  }

  if (nrow(grad_notes) > 0) {
    message("  Making grad notes")
    grad_note_items <- grad_notes$homework_notes %>%
      stringr::str_trim("right") %>% stringr::str_c(collapse = "\n\n")
  } else {
    grad_note_items <- NULL
  }

  if (c(everyone_note_items, ugrad_note_items, grad_note_items) %>%
      purrr::map_lgl(is.null) %>% all() %>% not()) {
    message("  Appending notes to content")
    output <- output %>% stringr::str_trim() %>%
      stringr::str_c("### Notes on Homework:", "", sep = "\n\n")

    if (c(ugrad_note_items, grad_note_items) %>%
        purrr::map_lgl(is.null) %>% all()) {
      output <- stringr::str_c(output, everyone_note_items, sep = "\n")
    } else {
      if (! is.null(everyone_note_items)) {
        everyone_note_items <- stringr::str_c("**Everyone:** ",
                                              everyone_note_items,
                                              collapse = "\n")
      }
      if (! is.null(ugrad_note_items)) {
        ugrad_note_items <- stringr::str_c("**Undergraduates:** ",
                                           ugrad_note_items, collapse = "\n")
      }
      if (! is.null(grad_note_items)) {
        grad_note_items <- stringr::str_c("**Graduate Students:** ",
                                          grad_note_items, collapse = "\n")
      }
      notes <- c(everyone_note_items, ugrad_note_items, grad_note_items) %>%
        itemize()
      output <- stringr::str_c(output, adj_nl(notes, TRUE, 1), sep = "\n")
    }
  }
  output
}

make_hw_asgt_page <- function(key, semester, use_solutions = FALSE) {
  assignment <- get_hw_assignment(key, semester)

  hw_date <- assignment$date
  hw_topic <- assignment$hw_topic
  hw_idx <- assignment$hw_id
  hw_num <- assignment$hw_num
  hw_slug <- make_hw_slug(assignment)
  hw_type <- assignment$hw_type
  short_hw_type = assignment$short_hw_type
  pub_date <- semester$semester_dates$pub_date

  message("Making homework page for HW #", hw_num, " (index = ", hw_idx,
          ", slug = ", hw_slug, ")")

  delim <- "---"
  header <- tibble::tibble(title = hw_topic,
                           due_date = lubridate::as_date(hw_date) %>% as.character(),
                           assignment_type = hw_type,
                           short_assignment_type = short_hw_type,
                           assignment_number = hw_num, weight = hw_idx,
                           slug = hw_slug,
                           pubdate = as.character(pub_date),
                           date = as.character(hw_date),
                           output = list("blogdown::html_page" =
                                           list(md_extensions = get_md_extensions()))
  ) %>% purrr::discard(is_mt_or_na) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>% # nolint
    stringr::str_c(delim, ., delim, sep = "\n")
  context <- make_context(assignment, "homework", semester)
  hw_page <- stringr::str_c(
    header,
    make_hw_asgt_content(key, semester, use_solutions),
    sep = "\n"
  ) %>% expand_codes(context, semester)
  invisible(hw_page)
}

generate_hw_assignment <- function(key, semester, use_solutions = FALSE) {
  assignment <- get_hw_assignment(key, semester)

  hw_page <- make_hw_asgt_page(key, semester, use_solutions)

  hw_slug <- make_hw_slug(assignment)
  hw_fname <- stringr::str_c(hw_slug, ".Rmd")
  message("Making homework page for assignment ",
          ifelse(is.na(assignment$hw_num), assignment$hw_grp_key,
                 stringr::str_c("# ", assignment$hw_num)),
          " (index = ", assignment$hw_id,
          ", slug = ", hw_slug, ", filename = ", hw_fname, ")")
  hw_path <- hw_fname %>% file.path(semester$root_dir,
                                    "content", "assignment", .)
  hw_url <- hw_fname %>% stringr::str_replace("\\.Rmd$", "")
  cat(hw_page, file = hw_path)
  c(page = hw_page, url = hw_url)
}

make_short_hw_assignment <- function(key, semester) {
  assignment <- get_hw_assignment(key, semester)

  items <- semester$hw_items %>%
    dplyr::filter(.data$hw_grp_key == key) %>%
    # merge_dates(semester) %>%
    dplyr::arrange(.data$hw_item_id)

  # d <- assignment$date %>% unique()
  hw <- items %>%
    dplyr::mutate(short_homework = ifelse(is.na(.data$short_homework),
                                          .data$homework, .data$short_homework)) %>%
    dplyr::filter(!.data$hw_prologue, !.data$hw_epilogue,
                  ! is.na(.data$short_homework)) %>%
    dplyr::arrange(.data$undergraduate_only, .data$graduate_only,
                   dplyr::desc(.data$hw_self_assess),
                   dplyr::desc(.data$hw_optional),
                   .data$hw_item_id)
  hw_topics <- hw %>% dplyr::mutate(topic = stringr::str_trim(.data$short_homework))

  if (any(hw_topics$undergraduate_only | hw_topics$graduate_only)) {
    hw_topics <- hw_topics %>%
      dplyr::mutate(topic = stringr::str_c(.data$topic, " (",
                                           ifelse(.data$undergraduate_only, "undergrads",
                                                  ifelse(.data$graduate_only, "grad. students",
                                                         "everyone")),
                                           ")"))
  }
  hw_topics <- hw_topics$topic
  if (length(hw_topics) > 1) {
    if (length(hw_topics) > 2) {
      hw_topics <- hw_topics %>%
        {
          c( head(., -1) %>% stringr::str_c(collapse = ", "), tail(., 1)) %>%
            stringr::str_c(collapse = ", and ")
        }
    } else {
      hw_topics <- stringr::str_c(hw_topics, collapse = " and ")
    }
  }
  output <- NULL
  if (length(hw_topics > 0)) {
    output <- stringr::str_c( "Homework #", assignment$hw_num,
                              " is due today: ", add_period(hw_topics),
                              " See the homework assignment sheet for details.") %>%
      stringr::str_c( "## Homework", "", .,  "", sep = "\n" )
  }
  output
}

