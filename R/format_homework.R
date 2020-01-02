get_hw_assignment <- function(key, semester) {
  assignment <- semester$hw_asgt %>% dplyr::filter(hw_key == key)
  assertthat::assert_that(nrow(assignment) == 1,
                          msg = stringr::str_c(
                            "There should only be one homework assignment for a given key: ",
                            "key ", key, " has ", nrow(assignment), " assignments.")
  )

  assignment <- as.list(assignment)
  assignment
}

make_hw_slug <- function(hw_asgt) {
  message("Making HW slug for ", hw_asgt$hw_key,
          ", is_numbered = ", hw_asgt$is_numbered,
          ", hw_num = ", hw_asgt$hw_num)
  if (hw_asgt$is_numbered) {
    slug <- sprintf("homework_%02d", hw_asgt$hw_num)
  } else {
    slug <- hw_asgt$slug
  }
  slug
}

make_hw_solution_page <- function(solution, semester, slug = NA_character_) {
  dbg_checkpoint(g_hw_sol, solution)

  if (is.null(slug) || is.na(slug)) {
    slug = sprintf("homework_%02d", solution$hw_num)
  }

  message("Generating markdown for solutions to homework #", assignment$hw_num,
          ", slug = ", slug)

  delim <- "---"
  header <- list(
    title = solution$hw_sol_title,
    hw_number = solution$hw_num,
    pubdate = as.character(solution$hw_sol_pub_date),
    date = as.character(solution$hw_due_date),
    pdf_url = solution$hw_sol_pdf_url,
    slug = stringr::str_c(slug, "_", solution$hw_sol_filename)) %>%
    purrr::discard(is.na) %>%
    c(
      output = list("blogdown::html_page" =
                      list(md_extensions = get_md_extensions(),
                           toc = TRUE))
    ) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  context <- make_context(solution, "homework solution", semester)
  hw_solution_page <- stringr::str_c(
    header,
    solution$hw_sol_markdown,
    sep = "\n"
  ) %>% expand_codes(context, semester)
  hw_solution_page
}

make_hw_solution <- function(solution, assignment, semester, slug = NA_character_) {
  if (is.na(slug)) {
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

make_hw_asgt_content <- function(key, semester, use_solutions = FALSE) {
  assignment <- get_hw_assignment(key, semester)

  items <- semester$hw_items %>% dplyr::filter(hw_key == key) %>%
    # merge_dates(semester) %>%
    dplyr::arrange(hw_item_id)
  if (use_solutions) {
    solutions <- semester$hw_sol %>% dplyr::filter(hw_key == key)

    dbg_checkpoint(g_hw_asgt, assignment)
    dbg_checkpoint(g_sol, solutions)

    solutions <- solutions %>%
      dplyr::mutate( due_cal_id = assignment$due_cal_id,
                     due_date = assignment$due_date) %>%
      # merge_dates(semester, id_col = "sol_pub_cal_id",
      #             date_col = "sol_pub_date") %>%
      dplyr::mutate(sol_pub_date =
                      lubridate::as_datetime(sol_pub_date,
                                             tz = get_semestr_tz())) %>%
      dplyr::filter(sol_pub_date <= lubridate::now()) %>%
      dplyr::arrange(lab_sol_id)
  }

  hw <- items %>%
    dplyr::filter(! is.na(homework) & stringr::str_length(homework) > 0)
  hw_a <- hw %>% dplyr::filter(!prologue, !epilogue)
  grad_hw <- hw_a %>% dplyr::filter(graduate_only)
  ugrad_hw <- hw_a %>% dplyr::filter(undergraduate_only)
  everyone_hw <- hw_a %>% dplyr::filter(!graduate_only & ! undergraduate_only)

  prologue <- hw %>% dplyr::filter(prologue)
  epilogue <- hw %>% dplyr::filter(epilogue)

  notes <- hw %>% dplyr::filter(! is.na(homework_notes))
  main_notes <- notes %>% dplyr::filter(! (prologue | epilogue))
  grad_notes <- main_notes %>% dplyr::filter(graduate_only)
  ugrad_notes <- main_notes %>% dplyr::filter(undergraduate_only)
  everyone_notes <- main_notes %>%
    dplyr::filter(!graduate_only & ! undergraduate_only)
  prologue_notes <- notes %>% dplyr::filter(prologue)
  epilogue_notes <- notes %>% dplyr::filter(epilogue)

  output <- ""

  if (! is.null(solutions) && nrow(solutions) >= 1) {
    message("Making homework solutions")
    output <- output %>% stringr::str_c("## Solutions:\n\n")
    for (i in seq(nrow(solutions))) {
      this_sol <- solutions[i,]
      sol <- make_hw_solution(this_sol, assignment, semester, slug)
      output <- output %>% stringr::str_c("* [", this_sol$hw_sol_title, "](",
                                          sol['url'], ")\n")
    }
    output <- stringr::str_c(output, "\n")
  }

  output <- stringr::str_c(output, "## Homework")
  if (nrow(prologue) > 0) {
    prologue_str <-
      stringr::str_c(purrr::discard(prologue$homework,
                                    ~is.na(.x) | .x == "") %>%
                       unique(),
            collapse = "\n\n")
  } else {
    prologue_str <- NULL
  }

  if (nrow(epilogue) > 0) {
    epilogue_str <-
      stringr::str_c(purrr::discard(epilogue$homework,
                                    ~is.na(.x) | .x == "") %>%
                       unique(),
            collapse = "\n\n")
  } else {
    epilogue_str <-  NULL
  }

  output <- stringr::str_c(output, prologue_str, "", "", sep = "\n")
  if (nrow(ugrad_hw) > 0) {
    ugrad_hw_items <- ugrad_hw$homework %>% unique() %>% itemize() %>%
      stringr::str_c(stringr::str_c("**Undergraduate Students",
                                    ifelse(nrow(everyone_hw) > 0,
                                           ",** also do the following:",
                                           ":**")),
                     ., sep = "\n")
  } else {
    ugrad_hw_items <- NULL
  }
  if (nrow(grad_hw) > 0) {
    grad_hw_items <- grad_hw$homework %>% unique() %>% itemize() %>%
      stringr::str_c(stringr::str_c("**Graduate Students",
                                    ifelse(nrow(everyone_hw) > 0,
                                           ",** also do the following:",
                                           ":**")),
                     ., sep = "\n")
  } else {
    grad_hw_items <- NULL
  }
  if (nrow(everyone_hw) > 0) {
    everyone_hw_items <- everyone_hw$homework %>% unique() %>% itemize()
    if (! all(is.null(grad_hw_items), is.null(ugrad_hw_items))) {
      everyone_hw_items <- stringr::str_c("**Everyone:**", everyone_hw_items,
                                          sep = "\n")
    }
  } else {
    everyone_hw_items <- NULL
  }
  if (all(is.null(grad_hw_items), is.null(ugrad_hw_items))) {
    output <- stringr::str_c(stringr::str_trim(output), "",
                    everyone_hw_items,
                    "", sep = "\n")
  } else {
    output <- stringr::str_c(stringr::str_trim(output), "",
                    itemize(c(everyone_hw_items, ugrad_hw_items, grad_hw_items)),
                    "", sep = "\n")
  }

  output <- stringr::str_c(stringr::str_trim(output), "",
                  epilogue_str, "",
                  sep = "\n"
  )

  everyone_notes <- dplyr::bind_rows(prologue_notes, everyone_notes, epilogue_notes) %>%
    dplyr::distinct()

  if (nrow(everyone_notes) > 0) {
    everyone_note_items <- everyone_notes$homework_notes %>%
      stringr::str_trim("right") %>% stringr::str_c(collapse = "\n\n")
  } else {
    everyone_note_items <- NULL
  }

  if (nrow(ugrad_notes) > 0) {
    ugrad_note_items <- ugrad_notes$homework_notes %>%
      stringr::str_trim("right") %>% stringr::str_c(collapse = "\n\n")
  } else {
    ugrad_note_items <- NULL
  }

  if (nrow(grad_notes) > 0) {
    grad_note_items <- grad_notes$homework_notes %>%
      stringr::str_trim("right") %>% stringr::str_c(collapse = "\n\n")
  } else {
    grad_note_items <- NULL
  }

  if (c(everyone_note_items, ugrad_note_items, grad_note_items) %>%
      purrr::map_lgl(is.null) %>% all() %>% not()) {
    output <- output %>% stringr::str_trim() %>%
      stringr::str_c("", "### Notes on Homework:", "", sep = "\n")

    if (c(ugrad_note_items, grad_note_items) %>%
        purrr::map_lgl(is.null) %>% all()) {
      output <- stringr::str_c(output, everyone_note_items, sep = "\n")
    } else {
      everyone_note_items <- stringr::str_c("**Everyone:**",
                                            everyone_note_items,
                                            collapse = "\n")
      ugrad_note_items <- stringr::str_c("**Undergraduates:**",
                                         ugrad_note_items, collapse = "\n")
      grad_note_items <- stringr::str_c("**Graduate Students:**",
                                        grad_note_items, collapse = "\n")
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
  hw_topic <- assignment$topic
  hw_idx <- assignment$hw_id
  hw_num <- assignment$hw_num
  hw_slug <- make_hw_slug(assignment)
  hw_type = assignment$hw_type
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
  ) %>% purrr::discard(is.na) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
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
          ifelse(is.na(assignment$hw_num), assignment$hw_key,
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
  get_hw_assignment(key, semester)

  items <- semester$hw_items %>% dplyr::filter(hw_key == key) %>%
    # merge_dates(semester) %>%
    dplyr::arrange(hw_item_id)

  d <- assignment$date %>% unique()
  hw <- items %>%
    dplyr::mutate(short_homework = ifelse(is.na(short_homework),
                                          homework, short_homework)) %>%
    dplyr::filter(!prologue, !epilogue, ! is.na(short_homework)) %>%
    dplyr::arrange(undergraduate_only, graduate_only, hw_item_id)
  hw_topics <- hw %>% dplyr::mutate(topic = stringr::str_trim(short_homework))

  if (any(hw_topics$undergraduate_only | hw_topics$graduate_only)) {
    hw_topics <- hw_topics %>%
      dplyr::mutate(topic = stringr::str_c(topic, " (",
                                  ifelse(undergraduate_only, "undergrads",
                                         ifelse(graduate_only, "grad. students",
                                                "everyone")),
                                  ")"))
  }
  hw_topics <- hw_topics %$% topic
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

