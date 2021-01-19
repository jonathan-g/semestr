#' Build an RMarkdown output format for PDF files.
#'
#' Build an RMarkdown output format for PDF files.
#'
#' @param header Metadata from the RMarkdown file.
#'
#' @return An RMarkdown output format object.
#'
#' @export
build_pdf_output_format <- function(header) {
  if (tibble::has_name(header, "output") && tibble::has_name(header$output, "pdf_document")) {
    output_options <- header$output$pdf_document
  } else {
    output_options <- list()
  }
  if (! tibble::has_name(output_options, "toc"))
    output_options$toc <- FALSE
  if (! tibble::has_name(output_options, "md_extensions"))
    output_options$md_extensions <- get_md_extensions()
  doc <- eval(expr(rmarkdown::pdf_document(!!!output_options)))
  invisible(doc)
}

#' Extract the local PDF filename corresponding to an URL.
#'
#' Extract the path to a local PDF file corresponding to an URL on the web site.
#'
#' @param pdf_url The URL (in the destination web site).
#' @param root_dir The root directory of the local HUGO project.
#' @param static_path Relative path to the HUGO static files.
#' @param force_dest Create missing directories?
#' @param ignore_missing_dest Ignore missing directories.
#' @param verbose Provide informative messages and warnings.
#'
#' @return The filename
#'
#' @export
pdf_filename <- function(pdf_url, root_dir, static_path = "static",
                         force_dest = FALSE, ignore_missing_dest = FALSE,
                         verbose = FALSE) {
  # message("root = ", root_dir, ", static = ", static_path,
  #         ", URL = ", pdf_url)
  if (is.na(pdf_url)) return(NA_character_)
  dest_dir <- file.path(root_dir, static_path) %>%
    cat_path(dirname(pdf_url)) %>% normalizePath(winslash = "/")
  # message("testing for dest path ", dest_dir)
  if (! dir.exists(dest_dir))
    if (force_dest) {
      if (verbose) message("Creating path ", dest_dir)
      dir.create(dest_dir, recursive = TRUE)
    } else if (!ignore_missing_dest) {
      warning("Destination directory does not exist: ", dest_dir)
      return(NA_character_)
    }
  dest <- file.path(dest_dir, basename(pdf_url))
  # message("Dest file = ", dest)
  dest
}

#' Build a PDF from an RMarkdown source file
#'
#' Build a PDF from an RMarkdown source file
#'
#' @param source_file The RMarkdown source file
#' @param root_dir The root directory of the HUGO project.
#' @param static_path Relative path to the HUGO static directory.
#' @param force_dest Create any missing directories.
#'
#' @return The path to the resulting PDF file.
#'
#' @export
build_pdf_from_rmd <- function(source_file, root_dir, static_path = "static",
                               force_dest = FALSE) {
  # message("building file ", source_file)
  hdr <- grab_header(source_file)
  if (tibble::has_name(hdr, "pdf_url")) {
    pdf_dest <- pdf_filename(hdr$pdf_url, root_dir, static_path, force_dest)
    if (is.na(pdf_dest)) {
      message("Invalid pdf URL in header: ", hdr$pdf_url)
      return(NA_character_)
    }
  } else {
    # message("No pdf output declared")
    return(NA_character_)
  }
  pdf_output <- build_pdf_output_format(hdr)

  message("building ", pdf_dest, " from ", basename(source_file))

  result <- rmarkdown::render(source_file, output_format = pdf_output,
                              output_file = pdf_dest)
  result
}

#' Build PDF versions of assignments.
#'
#' Build PDF versions of assignments RMarkdown.
#'
#' @param semester A list of data returned from `\link{load_semester_db}`
#' @param content_path A relative path to the HUGO content directory.
#' @param static_path Relative path to the HUGO static directory.
#' @param force_dest Create any missing directories.
#'
#' @return A vector of paths to the resulting PDF files.
#'
#' @export
build_pdf_files <- function(semester, content_path = "content",
                            static_path = "static", force_dest = TRUE) {
  root_dir <- semester$root_dir
  if (! dir.exists(content_path)) {
    content_path = cat_path(root_dir, content_path)
  }
  source_paths <- c("labs", "lab_docs", "assignment", "reading",
                    "homework_solutions", "lab_solutions")
  files <- list.files(file.path(content_path, source_paths), pattern = "*.Rmd",
                      full.names = TRUE)
  dest_paths <- character(0)
  for (f in files) {
    dest <- build_pdf_from_rmd(f, root_dir, static_path, force_dest)
    dest_paths <- c(dest_paths, dest)
  }

  invisible(dest_paths)
}
