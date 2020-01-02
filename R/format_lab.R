make_lab_slug <- function(lab_asgt) {
  message("Making lab slug for ", lab_asgt$lab_key,
          ", lab_num = ", lab_asgt$lab_num)
  slug <- sprintf("lab_%02d", lab_asgt$lab_num)
  slug
}

get_lab_assignment <- function(key, semester) {
  assignment <- semester$lab_asgt %>% dplyr::filter(lab_key == key)
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
  adj_nl(sol$sol_markdown)
}

make_lab_solution_page <- function(sol, semester) {
  dbg_checkpoint(g_sol, sol)
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
    purrr::discard(is.na) %>%
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
  dbg_checkpoint(g_sol, sol)

  fname <- sprintf("lab_%02d_%s.Rmd", sol$lab_num, sol$sol_filename)
  solution_path <- fname %>%
    file.path(semester$root_dir, "content", "lab_solutions/", .)
  solution_url <- fname %>% stringr::str_replace("\\.Rmd$", "") %>%
    file.path("/lab_solutions", .)
  lab_solution_page <- make_lab_solution_page(sol, semester)
  cat(lab_solution_page, file = solution_path)
  c(title = sol$sol_title, key = sol$lab_key, path = solution_path,
    url = solution_url)
}

make_lab_doc_content <- function(doc, semester) {
  doc$markdown
}

make_lab_doc_page <- function(doc, semester) {
  doc <- as.list(doc)
  dbg_checkpoint(g_doc, doc)

  delim <- "---"
  header <- list(
    title = doc$document_title,
    author = doc$doc_author,
    lab_number = doc$lab_num,
    lab_date = lubridate::as_date(doc$date) %>% as.character(),
    pubdate = as.character(semester$semester_dates$pub_date),
    date = as.character(doc$date),
    bibliography = doc$bibliography,
    pdf_url = doc$document_pdf_url,
    slug = sprintf("lab_%02d_%s", doc$lab_num, doc$doc_filename)) %>%
    purrr::discard(is.na) %>%
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
  c(title = lab$document_title, key = lab$lab_key, path = doc_path, url = doc_url)
}

make_lab_docs <- function(lab_key, semester) {
  lab_key <- enquo(lab_key)
  labs <- semester$lab_items %>%
    dplyr::filter(lab_key == !!lab_key, ! is.na(doc_filename)) # %>%
    # merge_dates(semester)

  lab_docs <- purrr::map( purrr::transpose(labs), ~make_lab_doc(.x, semester))
  dbg_checkpoint(g_lab_docs_out, lab_docs)
  invisible(lab_docs)
}

make_lab_assignment_content <- function(key, semester, use_solutions = FALSE) {
  assignment <- get_lab_assignment(key, semester)

  output <- adj_nl("# Overview:", assignment$description, start_par = TRUE,
                   extra_lines = 1)

  docs <- semester$lab_items %>% dplyr::filter(lab_key == key) %>%
    # merge_dates(semester) %>%
    dplyr::arrange(lab_item_id)

  dbg_checkpoint(g_lab_key, key)
  dbg_checkpoint(g_docs, docs)

  output <- cat_nl(output, "## Reading", TRUE)
  if (nrow(docs) > 0) {
    output <- cat_nl(output,
                     stringr::str_c("**Before you come to lab**, please read the following document",
                                    ifelse(nrow(docs) > 1, "s", ""), ":"),
                     extra_lines = 1)
    doc_links <- make_lab_docs(key, semester)
    dbg_checkpoint(g_doc_links, doc_links)
    if (is.list(doc_links)) {
      doc_links <- purrr::transpose(doc_links)
    } else {
      doc_links <- as_list(doc_links)
    }
    doc_items <- stringr::str_c("[", doc_links$title, "](",
                                doc_links$url, '){target="_blank"}') %>%
      itemize()
    output <- cat_nl(output, doc_items)
  } else {
    output <- cat_nl(output, "No reading has been posted yet for this lab.")
  }
  url <- assignment$assignment_url
  output <- cat_nl(output, "## Assignment", start_par = TRUE, extra_lines = 1)
  if (! is.na(url)) {
    output <- cat_nl(output,
                     stringr::str_c("Accept the assignment at GitHub Classroom at <",
                                    url, ">."))
  } else {
    output <- cat_nl(output, "## Assignment",
                     "The GitHub Classroom has not been posted yet.",
                     start_par = TRUE)
  }

  if (use_solutions) {

    solutions <- semester$lab_sol %>% dplyr::filter(lab_key == key) %>%
      # merge_dates(semester) %>%
      # merge_dates(semester, id_col = "sol_pub_cal_id",
      #             date_col = "sol_pub_date") %>%
      dplyr::filter(sol_pub_date <= lubridate::now()) %>%
      dplyr::arrange(sol_pub_date)

    if (nrow(solutions) > 0) {
      output <- cat_nl(output, "## Solutions",
                       "**Solutions for Lab Exercises**:",
                       start_par = TRUE)
      sol_links <- purrr::map(purrr::transpose(solutions),
                              ~make_lab_solution(.x, semester))
      if (is.list(sol_links)) {
        sol_links <- purrr::transpose(sol_links)
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
  lab_title <- assignment$title
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
  ) %>% purrr::discard(is.na) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
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


