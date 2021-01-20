make_lab_slug <- function(lab_asgt) {
  message("Making lab slug for ", lab_asgt$lab_key,
          ", lab_num = ", lab_asgt$lab_num)
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

make_lab_solution_page <- function(sol, semester) {
  sol <- as.list(sol)
  delim <- "---"
  header <- list(
    title = sol$sol_title,
    author = sol$sol_author,
    lab_number = sol$lab_num,
    lab_date = lubridate::as_date(sol$lab_date) %>% as.character(),
    sol_date = lubridate::as_date(sol$sol_pub_date) %>% as.character(),
    pubdate = as.character(sol$sol_pub_date),
    date = lubridate::as_date(sol$report_date) %>% as.character(),
    pdf_url = sol$sol_pdf_url,
    slug = sprintf("lab_%02d_%s", sol$lab_num,
                   sol$sol_filename)) %>%
    purrr::discard(is_mt_or_na) %>%
    c(
      output = list("blogdown::html_page" =
                      list(md_extensions = get_md_extensions(), toc = TRUE))
    ) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  context <- make_context(sol, "lab solution", semester)
  lab_solution_page <- cat_nl( header,
                               make_lab_solution_content(sol, semester)) %>%
    expand_codes(context, semester)
  lab_solution_page
}

make_lab_solution <- function(sol, semester) {
  fname <- sprintf("lab_%02d_%s.Rmd", sol$lab_num, sol$sol_filename)
  solution_path <- fname %>%
    file.path(semester$root_dir, "content", "lab_solutions/", .)
  solution_url <- fname %>% stringr::str_replace("\\.Rmd$", "") %>%
    file.path("/lab_solutions", .)
  lab_solution_page <- make_lab_solution_page(sol, semester)
  cat(lab_solution_page, file = solution_path)
  c(title = sol$sol_title, key = sol$lab_grp_key, path = solution_path,
    url = solution_url)
}

make_lab_doc_content <- function(doc, semester) {
  doc$lab_document_markdown
}

make_lab_doc_page <- function(doc, semester) {
  doc <- as.list(doc)
  delim <- "---"
  header <- list(
    title = doc$lab_document_title,
    author = doc$doc_author,
    lab_number = doc$lab_num,
    lab_date = as.character(doc$date),
    pubdate = as.character(semester$semester_dates$pub_date),
    date = as.character(doc$date),
    bibliography = doc$bibliography,
    pdf_url = doc$lab_document_pdf_url,
    slug = sprintf("lab_%02d_%s", doc$lab_num, doc$doc_filename)) %>%
    purrr::discard(is_mt_or_na) %>%
    c(
      output = list("blogdown::html_page" =
                      list(md_extensions = get_md_extensions(), toc = TRUE))
    ) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  context <- make_context(doc, "lab doc", semester)
  lab_doc_page <- cat_nl(header, make_lab_doc_content(doc, semester)) %>%
  expand_codes(context, semester)
  lab_doc_page
}

make_lab_doc <- function(lab, semester) {
  fname <- sprintf("lab_%02d_%s.Rmd", lab$lab_num, lab$doc_filename)
  doc_path <- fname %>% file.path(semester$root_dir, "content", "lab_docs", .)
  doc_url <- fname %>% stringr::str_replace("\\.Rmd$", "") %>%
    file.path("/lab_docs", .)
  lab_doc_page <- make_lab_doc_page(lab, semester)
  cat(lab_doc_page, file = doc_path)
  c(title = lab$lab_document_title, key = lab$lab_grp_key, path = doc_path, url = doc_url)
}

make_lab_docs <- function(lab_key, semester) {
  lab_key <- enquo(lab_key)
  labs <- semester$lab_items %>%
    dplyr::filter(.data$lab_grp_key == !!lab_key,
                  ! is.na(.data$doc_filename)) # %>%
    # merge_dates(semester)

  # purrr::pmap(list) is a nice way to transpose without screwing up classes
  # of date columns the way purrr::transpose() does.
  lab_docs <- labs %>%
    purrr::pmap(list) %>%
    purrr::map(~make_lab_doc(.x, semester))
  invisible(lab_docs)
}

make_lab_assignment_content <- function(key, semester, use_solutions = FALSE) {
  assignment <- get_lab_assignment(key, semester)

  output <- adj_nl("# Overview:", assignment$description, start_par = TRUE,
                   extra_lines = 1)

  docs <- semester$lab_items %>%
    dplyr::filter(.data$lab_grp_key == key) %>%
    # merge_dates(semester) %>%
    dplyr::arrange(.data$lab_item_id)
  output <- cat_nl(output, "## Reading", start_par = TRUE)
  if (nrow(docs) > 0) {
    output <- cat_nl(output,
                     stringr::str_c("**Before you come to lab**, please read the following document",
                                    ifelse(nrow(docs) > 1, "s", ""), ":"),
                     start_par = TRUE, extra_lines = 1)
    doc_links <- make_lab_docs(key, semester)
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
    output <- cat_nl(output, "No reading has been posted yet for this lab.")
  }
  url <- assignment$assignment_url
  output <- cat_nl(output, "## Assignment", start_par = TRUE, extra_lines = 1)
  if (! is_mt_or_na(url)) {
    output <- cat_nl(output,
                     stringr::str_c("Accept the assignment at GitHub Classroom at <",
                                    url, ">."))
  } else {
    output <- cat_nl(output, "The GitHub Classroom has not been posted yet.",
                     start_par = TRUE)
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
                              ~make_lab_solution(.x, semester))
      if (is.list(sol_links)) {
        sol_links <- purrr::pmap(sol_links, list)
      }
      output <- cat_nl(output,
                       stringr::str_c("[", sol_links$sol_title, "](",
                                      sol_links$url, '){target="_blank"}') %>%
                         itemize())
    }
  }
  context <- make_context(assignment, "lab", semester)
  output <- output %>% expand_codes(context, semester)
  output
}

make_lab_assignment_page <- function(key, semester, use_solutions = FALSE) {
  assignment <- get_lab_assignment(key, semester)

  lab_date <- assignment$date
  lab_title <- assignment$lab_title
  lab_idx <- assignment$lab_id
  lab_num <- assignment$lab_num
  lab_slug <- make_lab_slug(assignment)
  pub_date <- semester$semester_dates$pub_date

  message("Making lab page for Lab #", lab_num, " (index = ", lab_idx,
          ", slug = ", lab_slug, ")")

  delim <- "---"

  header <- list(
    title = assignment$title,
    lab_date = lubridate::as_date(assignment$date) %>% as.character(),
    presentation_date = lubridate::as_date(assignment$pres_date) %>% as.character(),
    report_due_date = as.character(assignment$report_date),
    lab_number = assignment$lab_num,
    github_classroom_assignment_url = assignment$assignment_url,
    pubdate = as.character(semester$semester_dates$pub_date),
    date = lubridate::as_date(assignment$date) %>% as.character(),
    slug = sprintf("lab_%02d_assignment", assignment$lab_num),
    output = list("blogdown::html_page" =
                    list(md_extensions = get_md_extensions()))
  ) %>% purrr::discard(is_mt_or_na) %>%
    yaml::as.yaml(.) %>%
    stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")

  context <- make_context(assignment, "lab", semester)
  lab_page <- stringr::str_c(
    header,
    make_lab_assignment_content(key, semester, use_solutions), sep = "\n") %>%
    expand_codes(context, semester)
  invisible(lab_page)
}

generate_lab_assignment <- function(key, semester, use_solutions = FALSE) {
  assignment <- get_lab_assignment(key, semester)

  lab_num <- assignment$lab_num

  fname <- sprintf("lab_%02d_assignment.Rmd", assignment$lab_num)
  lab_path <- fname %>% file.path(semester$root_dir, "content", "labs", .)
  lab_url <- fname %>% stringr::str_replace("\\.Rmd$", "")
  message("Making lab assignment page for lab # ", lab_num,
          " (index = ", assignment$lab_id,
          ", filename = ", fname, ")")
  lab_assignment_page <- make_lab_assignment_page(key, semester, use_solutions)
  cat(lab_assignment_page, file = lab_path)
  c(path = lab_path, url = lab_url)
}


