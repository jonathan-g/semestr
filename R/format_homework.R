make_hw_slug <- function(cal_entry) {
  message("Making HW slug for ", cal_entry$homework_id,
          ": has_hw = ", cal_entry$has_hw,
          ", has_numbered_hw = ", cal_entry$has_numbered_hw,
          ", hw_num = ", cal_entry$hw_num,
          ", hw_index = ", cal_entry$hw_index)
  if (cal_entry$has_numbered_hw) {
    slug <- sprintf("homework_%02d", cal_entry$hw_num)
  } else {
    slug <- cal_entry$hw_slug
  }
  slug
}

make_hw_solution_page <- function(solution, assignment, slug = NA_character_) {
  g_doc <<- solution
  g_asg <<- assignment

  if (is.na(slug)) {
    slug = sprintf("homework_%02d", assignment$hw_num)
  }

  message("Generating markdown for solutions to homework #", assignment$hw_num,
          ", slug = ", slug)

  delim <- "---"
  header <- list(
    title = solution$hw_sol_title,
    hw_number = assignment$hw_num,
    pubdate = as.character(solution$hw_sol_pub_date),
    date = as.character(assignment$hw_due_date),
    pdf_url = solution$hw_sol_pdf_url,
    slug = str_c(slug, "_", solution$hw_sol_filename)) %>%
    discard(is.na) %>%
    c(
      output = list("blogdown::html_page" =
                      list(md_extensions = md_extensions,
                           toc = TRUE))
    ) %>%
    as.yaml() %>% str_trim("right") %>%
    str_c(delim, ., delim, sep = "\n")
  hw_solution_page <- str_c(
    header,
    solution$hw_sol_markdown,
    sep = "\n"
  ) %>% expand_codes()
  hw_solution_page
}

make_hw_solution <- function(solution, assignment, slug = NA_character_) {
  if (is.na(slug)) {
    slug = sprintf("homework_%02d", assignment$hw_num)
  }
  fname <- str_c(slug, "_", solution$hw_sol_filename, ".Rmd")
  solution_path <- fname %>%
    file.path(root_dir, "content", "homework_solutions/", .)
  solution_url <- fname %>% str_replace("\\.Rmd$", "") %>%
    file.path("/homework_solutions", .)
  message("Making solutions file for homework #", assignment$hw_num, ": ",
          solution_path)
  hw_solution_page <- make_hw_solution_page(solution, assignment, slug)
  cat(hw_solution_page, file = solution_path)
  c(path = solution_path, url = solution_url)
}

make_hw_page <- function(cal_entry) {
  hw_date <- cal_entry$date
  this_assignment <- homework_assignments %>%
    dplyr::filter(homework_id == cal_entry$homework_id)
  hw_topic <- head(this_assignment$hw_topic, 1)
  hw_idx <- cal_entry$hw_index
  hw_num <- cal_entry$hw_num
  hw_slug <- make_hw_slug(cal_entry)
  hw_type = this_assignment$hw_type %>% unique()
  short_hw_type = this_assignment$short_hw_type %>% unique()
  if (length(hw_type) != 1) {
    stop("Error: multiple assignment types.")
  }
  if (length(short_hw_type) != 1) {
    stop("Error: multiple short assignment types: ", str_c(short_hw_type, collapse = ", "))
  }

  message("Making homework page for HW #", hw_num, " (index = ", hw_idx,
          ", slug = ", hw_slug, ")")

  delim <- "---"
  header <- tibble(title = hw_topic,
                   due_date = as.character(hw_date),
                   assignment_type = hw_type,
                   short_assignment_type = short_hw_type,
                   assignment_number = hw_num, weight = hw_idx,
                   slug = hw_slug,
                   pubdate = as.character(pub_date),
                   date = as.character(hw_date),
                   output = list("blogdown::html_page" =
                                   list(md_extensions = md_extensions))
  ) %>% discard(is.na) %>%
    as.yaml() %>% str_trim("right") %>%
    str_c(delim, ., delim, sep = "\n")
  hw_page <- str_c(
    header,
    make_homework_assignment(this_assignment, homework_solutions, hw_slug),
    sep = "\n"
  ) %>% expand_codes()
}

make_homework_assignment <- function(this_assignment,
                                     homework_solutions = NULL,
                                     slug = NA_character_) {
  if (! is.null(homework_solutions)) {
    solutions <- homework_solutions %>%
      dplyr::filter(as_datetime(hw_sol_pub_date) <= now())
  } else {
    solutions <- NULL
  }
  hw <- this_assignment %>%
    dplyr::filter(! is.na(homework) & str_length(homework) > 0)
  hw_a <- hw %>% dplyr::filter(!hw_prologue, !hw_epilogue)
  grad_hw <- hw_a %>% dplyr::filter(hw_graduate_only)
  ugrad_hw <- hw_a %>% dplyr::filter(hw_undergraduate_only)
  everyone_hw <- hw_a %>% dplyr::filter(!hw_graduate_only & ! hw_undergraduate_only)

  prologue <- hw %>% dplyr::filter(hw_prologue)
  epilogue <- hw %>% dplyr::filter(hw_epilogue)

  notes <- this_assignment %>% dplyr::filter(! is.na(homework_notes))
  main_notes <- notes %>% dplyr::filter(! (hw_prologue | hw_epilogue))
  grad_notes <- main_notes %>% dplyr::filter(hw_graduate_only)
  ugrad_notes <- main_notes %>% dplyr::filter(hw_undergraduate_only)
  everyone_notes <- main_notes %>%
    dplyr::filter(!hw_graduate_only & ! hw_undergraduate_only)
  prologue_notes <- notes %>% dplyr::filter(hw_prologue)
  epilogue_notes <- notes %>% dplyr::filter(hw_epilogue)

  output <- ""

  if (! is.null(solutions) && nrow(solutions) >= 1) {
    message("Making homework solutions")
    output <- output %>% str_c("## Solutions:\n\n")
    for (i in seq(nrow(solutions))) {
      this_sol <- solutions[i,]
      sol <- make_hw_solution(this_sol, this_assignment, slug)
      output <- output %>% str_c("* [", this_sol$hw_sol_title, "](", sol['url'], ")\n")
    }
    output <- str_c(output, "\n")
  }

  output <- str_c(output, "## Homework")
  if (nrow(prologue) > 0) {
    prologue_str <-
      str_c(discard(prologue$homework, ~is.na(.x) | .x == "") %>% unique(),
            collapse = "\n\n")
  } else {
    prologue_str <- NULL
  }

  if (nrow(epilogue) > 0) {
    epilogue_str <-
      str_c(discard(epilogue$homework, ~is.na(.x) | .x == "") %>% unique(),
            collapse = "\n\n")
  } else {
    epilogue_str <-  NULL
  }

  output <- str_c(output, prologue_str, "", "", sep = "\n")
  if (nrow(ugrad_hw) > 0) {
    ugrad_hw_items <- ugrad_hw$homework %>% unique() %>% itemize() %>%
      str_c("**Undergraduate Students:**", ., sep = "\n")
  } else {
    ugrad_hw_items <- NULL
  }
  if (nrow(grad_hw) > 0) {
    grad_hw_items <- grad_hw$homework %>% unique() %>% itemize() %>%
      str_c("**Graduate Students:**", ., sep = "\n")
  } else {
    grad_hw_items <- NULL
  }
  if (nrow(everyone_hw) > 0) {
    everyone_hw_items <- everyone_hw$homework %>% unique() %>% itemize()
    if (! all(is.null(grad_hw_items), is.null(ugrad_hw_items))) {
      everyone_hw_items <- str_c("**Everyone:**", everyone_hw_items, sep = "\n")
    }
  } else {
    everyone_hw_items <- NULL
  }
  if (all(is.null(grad_hw_items), is.null(ugrad_hw_items))) {
    output <- str_c(str_trim(output), "",
                    everyone_hw_items,
                    "", sep = "\n")
  } else {
    output <- str_c(str_trim(output), "",
                    itemize(c(everyone_hw_items, ugrad_hw_items, grad_hw_items)),
                    "", sep = "\n")
  }

  output <- str_c(str_trim(output), "",
                  epilogue_str, "",
                  sep = "\n"
  )

  everyone_notes <- bind_rows(prologue_notes, everyone_notes, epilogue_notes) %>%
    dplyr::distinct()

  if (nrow(everyone_notes) > 0) {
    everyone_note_items <- everyone_notes$homework_notes %>%
      str_trim("right") %>% str_c(collapse = "\n\n")
  } else {
    everyone_note_items <- NULL
  }

  if (nrow(ugrad_notes) > 0) {
    ugrad_note_items <- ugrad_notes$homework_notes %>%
      str_trim("right") %>% str_c(collapse = "\n\n")
  } else {
    ugrad_note_items <- NULL
  }

  if (nrow(grad_notes) > 0) {
    grad_note_items <- grad_notes$homework_notes %>%
      str_trim("right") %>% str_c(collapse = "\n\n")
  } else {
    grad_note_items <- NULL
  }

  if (c(everyone_note_items, ugrad_note_items, grad_note_items) %>%
      map_lgl(is.null) %>% all() %>% not()) {
    output <- output %>% str_trim() %>%
      str_c("", "### Notes on Homework:", "", sep = "\n")

    if (c(ugrad_note_items, grad_note_items) %>%
        map_lgl(is.null) %>% all()) {
      output <- str_c(output, everyone_note_items, sep = "\n")
    } else {
      everyone_note_items <- str_c("**Everyone:**", everyone_note_items, collapse = "\n")
      ugrad_note_items <- str_c("**Undergraduates:**", ugrad_note_items, collapse = "\n")
      grad_note_items <- str_c("**Graduate Students:**", grad_note_items, collapse = "\n")
      notes <- c(everyone_note_items, ugrad_note_items, grad_note_items) %>% itemize()
      output <- str_c(output, "",
                      append_newline_if_needed(notes),
                      "", sep = "\n")
    }
  }
  output
}

make_short_hw_assignment <- function(cal_entry) {
  d <- cal_entry$date
  homework <- homework_assignments %>%
    dplyr::filter(homework_id == cal_entry$homework_id) %>%
    dplyr::arrange(hw_undergraduate_only, hw_graduate_only, homework_id, hw_order)
  homework_topic <- homework %>% dplyr::filter(! is.na(short_homework)) %>%
    dplyr::mutate(topic = str_trim(short_homework))
  if (any(homework_topic$hw_undergraduate_only |
          homework_topic$hw_graduate_only)) {
    homework_topic <- homework_topic %>%
      dplyr::mutate(topic = str_c(topic, " (",
                                  ifelse(hw_undergraduate_only, "undergrads",
                                         ifelse(hw_graduate_only, "grad.\ students",
                                                "everyone")),
                                  ")"))
  }
  homework_topic <- homework_topic %$% topic
  if (length(homework_topic) > 1) {
    if (length(homework_topic) > 2) {
      homework_topic <- homework_topic %>%
        {
          c( head(., -1) %>% str_c(collapse = ", "), tail(., 1)) %>%
            str_c(collapse = ", and ")
        }
    } else {
      homework_topic <- str_c(homework_topic, collapse = " and ")
    }
  }
  output <- NULL
  if (length(homework_topic > 0)) {
    output <- str_c( "Homework #", cal_entry$hw_num, " is due today: ",
                     add_period(homework_topic),
                     " See the homework assignment sheet for details.") %>%
      str_c( "## Homework", "", .,  "", sep = "\n" )
  }
  output
}

