make_lab_slug <- function(lab_asgt) {
  if (getOption("semestr.verbose", default = 1) >= 2) {
    message("Making lab slug for ", lab_asgt$lab_grp_key,
            ", lab_num = ", lab_asgt$lab_num)
  }
  slug <- sprintf("lab_%02d", lab_asgt$lab_num)
  slug
}

get_lab_assignment <- function(key, semester) {
  assignment <- semester$lab_asgt %>%
    dplyr::filter(.data$lab_grp_key == key)
  assertthat::assert_that(nrow(assignment) == 1,
                          msg = stringr::str_c(
                            "There should only be one lab assignment for a given key: ",
                            "key ", key, " has ", nrow(assignment),
                            " assignments.")
  )

  assignment <- as.list(assignment)
  assignment
}

make_lab_solution_content <- function(sol, semester) {
  adj_nl(sol$lab_sol_markdown)
}

make_lab_solution_page <- function(sol, semester, schedule,
                                   use_pdfs = TRUE) {
  sol <- as.list(sol)
  delim <- "---"
  sol_slug <- sprintf("lab_%02d_%s", sol$lab_num, sol$lab_sol_filename)
  header <- list(
    title = sol$lab_sol_title,
    author = sol$lab_sol_author,
    lab_number = sol$lab_num,
    lab_date = lubridate::as_date(sol$lab_date) %>% as.character(),
    sol_date = lubridate::as_date(sol$sol_pub_date) %>% as.character(),
    pubdate = as.character(sol$sol_pub_date),
    date = lubridate::as_date(sol$report_date) %>% as.character(),
    slug = sol_slug
  )
  if (use_pdfs) {
    header$pdf_url <- file.path(semester$file_paths['lab_doc_pdf'],
                                stringr::str_c(sol_slug, ".pdf")) %>%
      clean_url()
  }
  header$output = list(
    "blogdown::html_page" =
      list(md_extensions = get_md_extensions(),
           toc = TRUE)
  )
  header <- header %>%
    purrr::discard(is_mt_or_na) %>%
    yaml::as.yaml() %>%
    stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n" )
  context <- make_context(sol, "lab solution", semester)
  lab_solution_page <- cat_nl( header,
                               make_lab_solution_content(sol, semester)) %>%
    expand_codes(context, semester, schedule)
  lab_solution_page
}

make_lab_solution <- function(sol, semester, schedule, use_pdfs = TRUE) {
  if (getOption("semestr.verbose", default = 1) >= 1) {
    message("Making solutions for lab ", sol$lab_num, ".")
  }
  fname <- sprintf("lab_%02d_%s.Rmd", sol$lab_num, sol$lab_sol_slug)
  solution_path <- file.path(semester$root_dir,
                             semester$file_paths['lab_sol_src'],
                             fname) %>%
    clean_path()
  solution_url <- stringr::str_replace("\\.Rmd$", "") %>%
    file.path(semester$file_paths['lab_sol_pdf'], fname) %>%
    clean_url()
  lab_solution_page <- make_lab_solution_page(sol, semester, schedule,
                                              use_pdfs)
  cat(lab_solution_page, file = solution_path)
  c(title = sol$lab_sol_title, key = sol$lab_grp_key,
    path = solution_path, url = solution_url)
}

make_lab_doc_content <- function(doc, semester) {
  doc$lab_document_markdown
}

make_lab_doc_page <- function(doc, semester, schedule, use_pdfs = TRUE) {
  doc <- as.list(doc)
  delim <- "---"
  doc_slug <- sprintf("lab_%02d_%s", doc$lab_num, doc$doc_filename)
  header <- list(
    title = doc$lab_document_title,
    author = doc$doc_author,
    lab_number = doc$lab_num,
    lab_date = as.character(doc$date),
    pubdate = as.character(semester$semester_dates$pub_date),
    date = as.character(doc$date),
    bibliography = doc$bibliography,
    slug = doc_slug
    )
  if (use_pdfs) {
    header$pdf_url = file.path(semester$file_paths['lab_doc_pdf'],
                               stringr::str_c(doc_slug, ".pdf")) %>%
      clean_url()
  }
  header$output = list(
    "blogdown::html_page" =
      list(md_extensions = get_md_extensions(),
           toc = TRUE)
  )
  header <- header %>%
    purrr::discard(is_mt_or_na) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  context <- make_context(doc, "lab doc", semester)
  lab_doc_page <- cat_nl(header,
                         make_lab_doc_content(doc, semester)) %>%
    expand_codes(context, semester, schedule)
  lab_doc_page
}

make_lab_doc <- function(lab, semester, schedule, use_pdfs = TRUE) {
  fname <- sprintf("lab_%02d_%s.Rmd", lab$lab_num, lab$doc_filename)
  doc_path <- file.path(semester$root_dir,
                        semester$file_paths['lab_doc_src'],
                        fname) %>%
    clean_path()
  doc_url <- file.path(semester$file_paths['lab_doc_dest'],
                       stringr::str_replace(fname, "\\.Rmd$", "")) %>%
    clean_url()
  lab_doc_page <- make_lab_doc_page(lab, semester, schedule, use_pdfs)
  cat(lab_doc_page, file = doc_path)
  c(title = lab$lab_document_title, key = lab$lab_grp_key,
    path = doc_path, url = doc_url)
}

make_lab_docs <- function(lab_key, semester, schedule, use_pdfs = TRUE) {
  labs <- semester$lab_items %>%
    dplyr::filter(.data$lab_grp_key == lab_key,
                  ! is.na(.data$doc_filename)) # %>%
  # merge_dates(semester)

  # purrr::pmap(list) is a nice way to transpose without screwing up classes
  # of date columns the way purrr::transpose() does.
  lab_docs <- labs %>%
    purrr::pmap(list) %>%
    purrr::map(~make_lab_doc(.x, semester, schedule, use_pdfs))
  invisible(lab_docs)
}

make_lab_assignment_content <- function(key, semester, schedule,
                                        use_solutions = FALSE,
                                        use_pdfs = TRUE) {
  assignment <- get_lab_assignment(key, semester)

  output <- cat_nl("# Overview:", assignment$lab_description,
                   start_par = TRUE, extra_lines = 1)

  docs <- semester$lab_items %>%
    dplyr::filter(.data$lab_grp_key == key) %>%
    # merge_dates(semester) %>%
    dplyr::arrange(.data$lab_item_id)
  output <- cat_nl(output, "## Reading", start_par = TRUE)
  if (nrow(docs) > 0) {
    output <- cat_nl(
      output,
      stringr::str_c("**Before you come to lab**, please read the following document",
                     ifelse(nrow(docs) > 1, "s", ""), ":"),
      start_par = TRUE, extra_lines = 1
    )
    doc_links <- make_lab_docs(key, semester, schedule, use_pdfs)
    if (is.list(doc_links)) {
      doc_links <- purrr::pmap(doc_links, list)
    } else {
      doc_links <- as_list(doc_links)
    }
    doc_items <- stringr::str_c("[", doc_links$title, "](",
                                doc_links$url, '){target="_blank"}') %>%
      itemize()
    output <- cat_nl(output, doc_items,  start_par = TRUE)
  } else {
    output <- cat_nl(output,
                     "No reading has been posted yet for this lab.")
  }
  url <- assignment$lab_assignment_url
  output <- cat_nl(output, "## Assignment", start_par = TRUE, extra_lines = 1)
  if (assignment$uses_gh_classroom) {
    if (! is_mt_or_na(url)) {
      output <- cat_nl(
        output,
        stringr::str_c("Accept the assignment at GitHub Classroom at <",
                       url, ">.")
      )
    } else {
      output <- cat_nl(
        output, "The GitHub Classroom has not been posted yet.",
        start_par = TRUE)
    }
  } else {
    output <- cat_nl(output, "This lab does not use GitHub Classroom.")
  }

  if (use_solutions) {

    solutions <- semester$lab_sol %>%
      dplyr::filter(.data$lab_grp_key == key) %>%
      # merge_dates(semester) %>%
      # merge_dates(semester, id_col = "sol_pub_cal_id",
      #             date_col = "sol_pub_date") %>%
      dplyr::filter(.data$sol_pub_date <= lubridate::now()) %>%
      dplyr::arrange(.data$sol_pub_date)

    if (nrow(solutions) > 0) {
      output <- cat_nl(output, "## Solutions",
                       "**Solutions for Lab Exercises**:",
                       start_par = TRUE)
      sol_links <- purrr::map(purrr::pmap(solutions, list),
                              ~make_lab_solution(.x, semester, schedule,
                                                 use_pdfs))
      if (is.list(sol_links)) {
        sol_links <- purrr::pmap(sol_links, list)
      }
      output <- cat_nl(
        output,
        stringr::str_c("[", sol_links$title, "](",
                       sol_links$url, '){target="_blank"}') %>%
          itemize(),
        start_par = TRUE
      )
    }
  }
  context <- make_context(assignment, "lab", semester)
  output <- output %>% expand_codes(context, semester, schedule)
  output
}

make_lab_assignment_page <- function(key, semester, schedule,
                                     use_solutions = FALSE,
                                     use_pdfs = TRUE) {
  assignment <- get_lab_assignment(key, semester)

  lab_date <- assignment$date
  lab_title <- assignment$lab_title
  lab_idx <- assignment$lab_id
  lab_num <- assignment$lab_num
  lab_slug <- make_lab_slug(assignment)
  pub_date <- semester$semester_dates$pub_date
  pres_date <- assignment$pres_date
  report_date <- assignment$report_date
  asgt_url <- assignment$lab_assignment_url

  if (getOption("semestr.verbose", default = 1) >= 1) {
    message("Making lab page for Lab #", lab_num, " (index = ", lab_idx,
            ", slug = ", lab_slug, ")")
  }

  delim <- "---"

  header <- list(
    title = lab_title,
    lab_date = as.character(lab_date),
    presentation_date = as.character(pres_date),
    report_due_date = as.character(report_date),
    lab_number = lab_num,
    github_classroom_assignment_url = asgt_url,
    pubdate = as.character(pub_date),
    date = as.character(lab_date),
    slug = lab_slug,
    output = list("blogdown::html_page" =
                    list(md_extensions = get_md_extensions()))
  ) %>% purrr::discard(is_mt_or_na) %>%
    yaml::as.yaml(.) %>%
    stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")

  context <- make_context(assignment, "lab", semester)
  lab_page <- stringr::str_c(
    header,
    make_lab_assignment_content(key, semester, schedule,
                                use_solutions, use_pdfs),
    sep = "\n"
  ) %>%
    expand_codes(context, semester, schedule)
  invisible(lab_page)
}

generate_lab_assignment <- function(key, semester, schedule,
                                    use_solutions = FALSE,
                                    use_pdfs = TRUE) {
  if (is.null(key) || is.na(key)) {
    return(c(path = NA_character_, url = NA_character_))
  }
  assignment <- get_lab_assignment(key, semester)

  lab_num <- assignment$lab_num

  fname <- sprintf("lab_%02d_assignment.Rmd", assignment$lab_num)
  lab_path <- file.path(semester$root_dir,
                        semester$file_paths['lab_asgt_src'],
                        fname) %>%
    clean_path()
  # lab_url <- fname %>% stringr::str_replace("\\.Rmd$", "")
  lab_url <- file.path(semester$file_paths['lab_asgt_dest'],
                       stringr::str_replace(fname, "_assignment\\.Rmd$", "")) %>%
    clean_url()
  if (getOption("semestr.verbose", default = 1) >= 1) {
    message("Making lab assignment page for lab # ", lab_num,
            " (index = ", assignment$lab_id,
            ", filename = ", fname, ")")
  }
  lab_assignment_page <-
    make_lab_assignment_page(key, semester, schedule, use_solutions,
                             use_pdfs)
  cat(lab_assignment_page, file = lab_path)
  c(path = lab_path, url = lab_url)
}


