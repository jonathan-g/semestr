make_lab_solution_page <- function(sol, semester,
                                   md_extensions = get_md_extensions()) {
  delim <- "---"
  header <- list(
    title = sol$sol_title,
    author = sol$sol_author,
    lab_number = sol$lab_num,
    lab_date = as_date(sol$lab_date) %>% as.character(),
    pubdate = as.character(sol$sol_pub_date),
    date = as_date(sol$report_date) %>% as.character(),
    pdf_url = sol$sol_pdf_url,
    slug = sprintf("lab_%02d_%s", sol$lab_num,
                   sol$sol_filename)) %>%
    purrr::discard(is.na) %>%
    c(
      output = list("blogdown::html_page" =
                      list(md_extensions = md_extensions,
                           toc = TRUE))
    ) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  lab_solution_page <- stringr::str_c(
    header,
    sol$sol_markdown,
    sep = "\n"
  ) %>% expand_codes(semester)
  lab_solution_page
}

make_lab_solution <- function(sol, semester,
                              md_extensions = get_md_extensions()) {
  fname <- sprintf("lab_%02d_%s.Rmd", sol$lab_num, sol$sol_filename)
  solution_path <- fname %>%
    file.path(semester$root_dir, "content", "lab_solutions/", .)
  solution_url <- fname %>% stringr::str_replace("\\.Rmd$", "") %>%
    file.path("/lab_solutions", .)
  lab_solution_page <- make_lab_solution_page(sol, semester, md_extensions)
  cat(lab_solution_page, file = solution_path)
  c(key = !!lab_key, path = solution_path, url = solution_url)
}

make_lab_doc_page <- function(doc, semester,
                              md_extensions = get_md_extensions()) {
  doc <- as.list(doc)
  dbg_checkpoint(g_doc, doc)

  delim <- "---"
  header <- list(
    title = doc$document_title,
    author = doc$doc_author,
    lab_number = doc$lab_num,
    lab_date = as_date(doc$date) %>% as.character(),
    pubdate = as.character(semester$semester_dates$pub_date),
    date = as.character(doc$date),
    bibliography = doc$bibliography,
    pdf_url = doc$document_pdf_url,
    slug = sprintf("lab_%02d_%s", doc$lab_num, doc$doc_filename)) %>%
    purrr::discard(is.na) %>%
    c(
      output = list("blogdown::html_page" =
                      list(md_extensions = md_extensions,
                           toc = TRUE))
    ) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  lab_doc_page <- stringr::str_c(
    header,
    doc$document_markdown,
    sep = "\n"
  ) %>% expand_codes(semester)
  lab_doc_page
}

make_lab_doc <- function(lab, semester, md_extensions = get_md_extensions()) {
  fname <- sprintf("lab_%02d_%s.Rmd", lab$lab_num, lab$doc_filename)
  doc_path <- fname %>% file.path(semester$root_dir, "content", "lab_docs", .)
  doc_url <- fname %>% stringr::str_replace("\\.Rmd$", "") %>%
    file.path("/lab_docs", .)
  lab_doc_page <- make_lab_doc_page(lab, semester, md_extensions)
  cat(lab_doc_page, file = doc_path)
  c(key = lab$lab_key, path = doc_path, url = doc_url)
}

make_lab_docs <- function(lab_key, semester,
                          md_extensions = get_md_extensions()) {
  lab_key <- enquo(lab_key)
  labs <- semester$lab_items %>%
    dplyr::filter(lab_key == !!lab_key, ! is.na(doc_filename)) %>%
    merge_dates(semester)

  lab_docs <- purrr::map( purrr::transpose(labs),
                          ~make_lab_doc(.x, semester, md_extensions))
  invisible(lab_docs)
}

make_lab_assignment_page <- function(assignment, docs, solutions, semester,
                                     md_extensions = get_md_extensions()) {
  delim <- "---"

  header <- list(
    title = assignment$title,
    lab_date = as_date(assignment$date) %>% as.character(),
    presentation_date = as_date(assignment$pres_date) %>% as.character(),
    report_due_date = as.character(assignment$report_date),
    lab_number = assignment$lab_num,
    github_classroom_assignment_url = assignment$assignment_url,
    pubdate = as.character(semester$semester_dates$pub_date),
    date = as_date(assignment$date) %>% as.character(),
    slug = sprintf("lab_%02d_assignment", assignment$lab_num),
    output = list("blogdown::html_page" =
                    list(md_extensions = md_extensions))
  ) %>% purrr::discard(is.na) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  output <- assignment$description
  if (nrow(docs) > 0) {
    output <- output %>% stringr::str_c(
      "", "## Reading", "",
      stringr::str_c("**Before you come to lab**, please read the following document",
            ifelse(nrow(docs) > 1, "s", ""),
            ":"), "",
      sep = "\n")
    for (i in seq(nrow(docs))) {
      this_doc <- docs[i,]
      doc_links <- make_lab_doc(this_doc, semester, md_extensions)
      output <- output %>% stringr::str_c("\n* [", this_doc$document_title,
                                          "](", doc_links['url'], ")")
    }
  } else {
    output <- output %>% stringr::str_c(
      "", "## Reading", "",
      "No reading has been posted yet for this lab.",
      "", sep = "\n")
  }
  url <- assignment$assignment_url
  if (! is.na(url)) {
    output <- output %>%
      stringr::str_c("", "## Assignment", "",
                     stringr::str_c("Accept the assignment at GitHub Classroom at <",
                                    url, ">."),
                     "", sep = "\n")
  } else {
    output <- output %>% stringr::str_c("", "## Assignment", "",
                               "The GitHub Classroom has not been posted yet.",
                               "", sep = "\n")
  }
  if (nrow(solutions) > 0) {
    output <- output %>% stringr::str_c(
      "", "## Solutions", "",
      "**Solutions for Lab Exercises**:", "",
      sep = "\n")
    for (i in seq(nrow(solutions))) {
      this_sol <- solutions[i,]
      sol_links <- make_lab_solution(this_sol, semester, md_extensions)
      output <- output %>%
        stringr::str_c("\n* [", this_sol$sol_title, "](", sol_links['url'], ")")
    }
  }
  output <- stringr::str_c(header, output, sep = "\n") %>%
    expand_codes(semester)
  output
}

make_lab_assignment <- function(key, semester,
                                md_extensions = get_md_extensions()) {
  assignment <- semester$lab_asgt %>% dplyr::filter(lab_key == key) %>%
    merge_dates(semester) %>%
    merge_dates(semester, id_col = "report_cal_id",
                date_col = "report_date") %>%
    merge_dates(semester, id_col = "pres_cal_id", date_col = "pres_date") %>%
    dplyr::arrange(lab_asg_id)
  docs <- semester$lab_items %>% dplyr::filter(lab_key == key) %>%
    merge_dates(semester) %>%
    dplyr::arrange(lab_item_id)
  solutions <- semester$lab_sol %>% dplyr::filter(lab_key == key) %>%
    dplyr::left_join( dplyr::select(assignment, lab_key,
                                    report_cal_id, report_date,
                                    pres_cal_id, pres_date), by = "lab_key") %>%
    merge_dates(semester, id_col = "sol_pub_cal_id",
                date_col = "sol_pub_date") %>%
    merge_dates(semester, date_col = "lab_date") %>%
    dplyr::mutate(sol_pub_date =
                    lubridate::as_datetime(sol_pub_date,
                                           tz = get_semestr_tz())) %>%
    dplyr::filter(sol_pub_date <= lubridate::now()) %>%
    dplyr::arrange(lab_sol_id)

  fname <- sprintf("lab_%02d_assignment.Rmd", assignment$lab_num)
  lab_path <- fname %>% file.path(semester$root_dir, "content", "labs", .)
  lab_url <- fname %>% stringr::str_replace("\\.Rmd$", "")
  lab_assignment_page <- make_lab_assignment_page(assignment, docs, solutions,
                                                  semester, md_extensions)
  cat(lab_assignment_page, file = lab_path)
  c(path = lab_path, url = lab_url)
}


