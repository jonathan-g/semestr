make_lab_solution_page <- function(solution, assignment) {
  # g_doc <<- solution
  # g_asg <<- assignment

  delim <- "---"
  header <- list(
    title = solution$lab_sol_title,
    author = solution$lab_sol_author,
    lab_number = assignment$lab_num,
    lab_date = as.character(assignment$lab_date),
    pubdate = as.character(solution$lab_sol_pub_date),
    date = as.character(assignment$lab_due_date),
    pdf_url = solution$lab_sol_pdf_url,
    slug = sprintf("lab_%02d_%s", assignment$lab_num, solution$lab_sol_filename)) %>%
    discard(is.na) %>%
    c(
      output = list("blogdown::html_page" =
                      list(md_extensions = md_extensions,
                           toc = TRUE))
    ) %>%
    as.yaml() %>% str_trim("right") %>%
    str_c(delim, ., delim, sep = "\n")
  lab_solution_page <- str_c(
    header,
    solution$lab_sol_markdown,
    sep = "\n"
  ) %>% expand_codes()
  lab_solution_page
}

make_lab_solution <- function(solution, assignment) {
  fname <- sprintf("lab_%02d_%s.Rmd", assignment$lab_num, solution$lab_sol_filename)
  solution_path <- fname %>% file.path(root_dir, "content", "lab_solutions/", .)
  solution_url <- fname %>% str_replace("\\.Rmd$", "") %>%
    file.path("/lab_solutions", .)
  lab_solution_page <- make_lab_solution_page(solution, assignment)
  cat(lab_solution_page, file = solution_path)
  c(path = solution_path, url = solution_url)
}

make_lab_doc_page <- function(doc, assignment) {
  # g_doc <<- doc
  # g_asg <<- assignment

  delim <- "---"
  header <- list(
    title = doc$lab_document_title,
    author = doc$doc_author,
    lab_number = assignment$lab_num,
    lab_date = as.character(assignment$lab_date),
    pubdate = as.character(pub_date),
    date = as.character(assignment$lab_date),
    bibliography = doc$bibliography,
    pdf_url = doc$lab_document_pdf_url,
    slug = sprintf("lab_%02d_%s", assignment$lab_num, doc$doc_filename)) %>%
    discard(is.na) %>%
    c(
      output = list("blogdown::html_page" =
                      list(md_extensions = md_extensions,
                           toc = TRUE))
    ) %>%
    as.yaml() %>% str_trim("right") %>%
    str_c(delim, ., delim, sep = "\n")
  lab_doc_page <- str_c(
    header,
    doc$lab_document_markdown,
    sep = "\n"
  ) %>% expand_codes()
  lab_doc_page
}

make_lab_doc <- function(doc, assignment) {
  fname <- sprintf("lab_%02d_%s.Rmd", assignment$lab_num, doc$doc_filename)
  doc_path <- fname %>% file.path(root_dir, "content", "lab_docs", .)
  doc_url <- fname %>% str_replace("\\.Rmd$", "") %>%
    file.path("/lab_docs", .)
  lab_doc_page <- make_lab_doc_page(doc, assignment)
  cat(lab_doc_page, file = doc_path)
  c(path = doc_path, url = doc_url)
}

make_lab_assignment_page <- function(this_assignment, lab_docs,
                                     lab_solutions) {
  delim <- "---"
  these_docs <- lab_docs %>% dplyr::filter(lab_group == this_assignment$lab_group) %>%
    dplyr::arrange(lab_item_id)
  these_solutions <- lab_solutions %>%
    dplyr::mutate(lab_sol_pub_date = as_datetime(lab_sol_pub_date)) %>%
    dplyr::filter(lab_group == this_assignment$lab_group, lab_sol_pub_date <= now()) %>%
    dplyr::arrange(lab_sol_id)

  header <- list(
    title = this_assignment$lab_title,
    lab_date = as.character(this_assignment$lab_date),
    presentation_date = as.character(this_assignment$lab_present_date),
    report_due_date = as.character(this_assignment$lab_due_date),
    lab_number = this_assignment$lab_num,
    github_classroom_assignment_url = this_assignment$lab_assignment_url,
    pubdate = as.character(pub_date),
    date = as.character(this_assignment$lab_date),
    slug = sprintf("lab_%02d_assignment", this_assignment$lab_num),
    output = list("blogdown::html_page" =
                    list(md_extensions = md_extensions))
  ) %>% discard(is.na) %>%
    as.yaml() %>% str_trim("right") %>%
    str_c(delim, ., delim, sep = "\n")
  output <- this_assignment$lab_description
  if (nrow(these_docs) > 0) {
    output <- output %>% str_c(
      "", "## Reading", "",
      str_c("**Before you come to lab**, please read the following document",
            ifelse(nrow(these_docs) > 1, "s", ""),
            ":"), "",
      sep = "\n")
    for (i in seq(nrow(these_docs))) {
      this_doc <- these_docs[i,]
      doc <- make_lab_doc(this_doc, this_assignment)
      output <- output %>% str_c("\n* [", this_doc$lab_document_title, "](", doc['url'], ")")
    }
  } else {
    output <- output %>% str_c(
      "", "## Reading", "",
      "No reading has been posted yet for this lab.",
      "", sep = "\n")
  }
  url <- this_assignment$lab_assignment_url
  if (! is.na(url)) {
    output <- output %>% str_c("", "## Assignment", "",
                               str_c("Accept the assignment at GitHub Classroom at <", url, ">."),
                               "", sep = "\n")
  } else {
    output <- output %>% str_c("", "## Assignment", "",
                               "The GitHub Classroom has not been posted yet.",
                               "", sep = "\n")
  }
  if (nrow(these_solutions) > 0) {
    output <- output %>% str_c(
      "", "## Solutions", "",
      "**Solutions for Lab Exercises**:", "",
      sep = "\n")
    for (i in seq(nrow(these_solutions))) {
      this_sol <- these_solutions[i,]
      sol <- make_lab_solution(this_sol, this_assignment)
      output <- output %>% str_c("\n* [", this_sol$lab_sol_title, "](", sol['url'], ")")
    }


  }
  output <- str_c(header, output, sep = "\n") %>% expand_codes()
  output
}

make_lab_assignment <- function(group, lab_assignments, lab_docs,
                                lab_solutions) {
  this_assignment <- lab_assignments %>% dplyr::filter(lab_group == group)
  fname <- sprintf("lab_%02d_assignment.Rmd", this_assignment$lab_num)
  lab_path <- fname %>% file.path(root_dir, "content", "labs", .)
  lab_url <- fname %>% str_replace("\\.Rmd$", "")
  lab_assignment_page <- make_lab_assignment_page(this_assignment, lab_docs,
                                                  lab_solutions)
  cat(lab_assignment_page, file = lab_path)
  c(path = lab_path, url = lab_url)
}


