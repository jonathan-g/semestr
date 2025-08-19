get_handout <- function(key, semester) {
  handout <- semester$handouts %>%
    dplyr::filter(.data$handout_key == key)
  assertthat::assert_that(nrow(handout) == 1,
                          msg = stringr::str_c(
                            "There should only be one handout for a given key: ",
                            "key ", key, " has ", nrow(handout),
                            " handouts.")
  )

  handout <- as.list(handout)
  handout
}

make_handout_content <- function(doc, semester) {
  doc$document_markdown
}

make_handout <- function(doc, semester, schedule) {
  doc <- as.list(doc)
  delim <- "---"
  slug = sprintf("%s", doc$doc_slug)
  header <- list(
    title = doc$doc_title,
    subtitle = "`r params$par_subtitle`",
    author = doc$doc_author,
    pubdate = as.character(semester$semester_dates$pub_date),
    date = "`r params$par_date",
    bibliography = doc$bibliography,
    slug = slug,
    pdf_url = file.path(semester$file_paths['handout_pdf'],
                        stringr::str_c(slug, ".pdf")) %>% clean_url(),
    params = list(
      par_date = lubridate::as_date(doc$date) %>% as.character(),
      par_subtitle = NULL
    )
  )
  header$output = list(
    "blogdown::html_page" =
      list(md_extensions = get_md_extensions(),
           toc = TRUE)
  )
  header <- header %>% purrr::discard(is_mt_or_na) %>%
    yaml::as.yaml() %>% stringr::str_trim("right") %>%
    stringr::str_c(delim, ., delim, sep = "\n")
  context <- make_context(doc, "handout", semester)
  handout_page <- cat_nl(header, make_handout_content(doc, semester))
  handout_page <- expand_codes(handout_page, context, semester,
                               schedule)
  handout_page
}

make_handout_page <- function(handout, semester, schedule) {
  fname <- sprintf("%s.Rmd", handout$doc_slug)
  doc_path <- file.path(semester$root_dir,
                        semester$file_paths['handout_src'], fname) %>%
                          clean_path()
  doc_url <-
    file.path(semester$file_paths['rd_asgt_dest'],
              stringr::str_replace(fname, "\\.Rmd$", "")) %>%
    clean_url()
  handout_content <- make_handout(handout, semester, schedule)
  if(getOption("semestr.verbose", default = 1) >= 1) {
    message("Writing handout page for ",
            handout$key, " to ", doc_path)
  }
  cat(handout_content, file = doc_path)
  c(title = handout$doc_title, key = handout$handout_key,
    path = doc_path, url = doc_url)
}


generate_handout <- function(key, semester, schedule) {
  if (is.null(key) || is.na(key)) {
    return(c(path = NA_character_, url = NA_character_))
  }
  handout <- get_handout(key, semester)

  fname <- sprintf("%s.Rmd", handout$doc_slug)
  handout_path <- file.path(semester$root_dir,
                            semester$file_paths['handout_src'],
                            fname) %>%
    clean_path()
  handout_url <- file.path(semester$file_paths['handout_dest'],
                           stringr::str_replace(fname, "\\.Rmd$", "")) %>%
    clean_url()
  if (getOption("semestr.verbose", default = 1) >= 1) {
    message("Making handout page for ", handout$doc_title,
            " (index = ", handout$handout_key,
            ", filename = ", fname, ")")
  }
  handout_page <- make_handout_page(handout, semester, schedule)
  c(path = handout_path, url = handout_url)
}

generate_handouts <- function(semester, schedule) {
  handouts <- semester$handouts$handout_key %>%
    purrr::map(~generate_handout(.x, semester, schedule))
  invisible(handouts)
}
