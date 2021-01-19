#' Get the digest algorithm to use
#'
#' \code{get_pdf_digest_algorithm} gets the digest algorithm that will be used.
#'
#' Set the algorithm with `options(blogdown.hash.algorithm = <algorithm>)`.
#' If the option is not set, then use crc32.
#'
#' @return A string containing the name of the algorithm.
#' @seealso \code{\link{digests}}.
#' @keywords internal
get_pdf_digest_algorithm <- function() {
  getOption("blogdownDigest.hash.algorithm", default = "crc32")
}

#' Calculate digest for pdf file
#'
#' Check whether a PDF file exists, and if so calculate its digest.
#'
#' @param file The pdf file (the file need not exist).
#' @param alg The digest algorithm to use.
#'
#' @return A character vector with the digest, or `NA` if the file does not
#'   exist.
#'
#' @export
pdf_digest_if_exists <- function (file, alg = NA_character_)
{
  if (file.exists(file)) {
    if (is.na(alg)) {
      alg <- get_pdf_digest_algorithm()
    }
    dgst <- digest::digest(file, file = TRUE, algo = alg)
  }
  else {
    dgst <- NA_character_
    alg <- NA_character_
  }
  c(digest = dgst, alg = alg)
}

#' Get URL corresponding to a generated
#'
#' From an RMarkdown source file, get the URL for downloading the PDF file that
#' will be generated from it.
#'
#' @param file The path to the RMarkdown source.
#'
#' @return The URL for the generated PDF file.
#'
#' @keywords internal
get_one_pdf_url <- function(file) {
  header <- grab_header(file)
  pdf_url <- NA_character_
  if (tibble::has_name(header, "pdf_url"))
    pdf_url <- header$pdf_url
  pdf_url
}

#' Get URLs corresponding to generated PDF files.
#'
#' From a collection of RMarkdown source files, get the URLs for downloading
#' the PDF files that will be generated from them.
#'
#' @param files A list or character vector of paths tor the RMarkdown
#'   source files.
#'
#' @return A character vector with the URLs for the generated PDF files.
#'
#' @export
get_pdf_url <- function(files) {
  purrr::map_chr(files, get_one_pdf_url)
}

#' Update digests for PDF files
#'
#' Update digests for all PDF files under the content path.
#'
#' If the `root_dir` is `NULL`, the function tries to find the root directory
#' using a few common heuristics.
#'
#' @param files Files to update.
#' @param root_dir The root directory for the project (Should be the root for
#'   a HUGO project).
#' @param static_path Where to look for static files (in the HUGO sense of
#'   static).
#' @param content_path Where to look for content (e.g., `.Rmd` and `.md` files).
#' @param partial Only update digests for new files without previous digests..
#'
#' @return A data frame with the digests.
#'
#' @export
update_pdf_file_digests <- function (files, root_dir, static_path = "static",
                                content_path = "content", partial = FALSE)
{
  if (is.null(root_dir)) {
    root_dir <- find_root_dir(use_globals = TRUE)
  }
  root_dir <- root_dir %>% normalizePath(winslash = "/")
  static_path <- strip_leading_slash(static_path)
  content_path <- strip_leading_slash(content_path)
  content_base <- file.path(root_dir, content_path)
  files <- files %>% purrr::discard(is.na) %>%
    normalizePath(winslash = "/") %>%
    unique() %>% purrr::keep(file.exists)
  dest_urls <- files %>% get_pdf_url()
  digests <- tibble::tibble( file = files,
                             dest = dest_urls) %>%
    dplyr::filter(! is.na(dest)) %>%
    dplyr::mutate(
      dest = purrr::map_chr(dest, ~pdf_filename(.x, root_dir = root_dir,
                                                static_path = static_path)),
      dgst = purrr::map(file, pdf_digest_if_exists),
      alg = purrr::map_chr(dgst, ~.x["alg"]),
      digest = purrr::map_chr(dgst, ~.x["digest"]),
      dest_digest = purrr::map2_chr(dest, alg,
                                    ~pdf_digest_if_exists(.x, .y)["digest"]),
      file = stringr::str_replace(file, stringr::fixed(root_dir), "~"),
      dest = stringr::str_replace(dest, stringr::fixed(root_dir), "~")
      )

  digest_file <- file.path(root_dir, "pdf_digests.Rds")
  if (partial && file.exists(digest_file)) {
    old_digests <- readr::read_rds(digest_file) %>%
      dplyr::filter(! file %in% digests$file)
    digests <- dplyr::bind_rows(digests, old_digests)
  }
  readr::write_rds(digests, digest_file)
  invisible(digests)
}


#' Check which files need to be rebuilt
#'
#' \code{pdf_needs_rebuild} returns a vector of logicals indicating which files
#' need to be rebuilt, based on whether the file has changed.
#'
#' This function compares digests of current files to stored digests in order
#' to tell whether the source file needs to be rebuilt.
#' If the digests are not equal, then the file has changed. If a digest is
#' missing, then the source file is new or the output file has been deleted
#' and in either case, the source file needs to be rebuilt.
#'
#' @param current_digest A character vector containing digests of the
#' current source files (\code{.Rmd} or \code{.rmarkdown}`).
#' @param current_dest_digest A character vector containing digests of the
#' current destination (output) files (\code{.html}`).
#' \code{NA} for destination files that do not exist.
#' @param old_digest The stored digest for the source file from the last time
#' the site was built. \code{NA} if the source file did not exist at the time
#' of the last build.
#' @param old_dest_digest A character vector containing stored digests for the
#' destination files from the last time the site was built.
#' \code{NA} for destination files that did not exist after the last build.
#' @return A vector of logicals indicating whether the destination (output)
#' files are out of date relative to the source files.
#'
#' If a destination file is missing or if any of the digests don't match,
#' then the file needs to be rebuilt.
#' @seealso \code{\link{digests}}.
#' @keywords internal
pdf_needs_rebuild <- function(current_digest, current_dest_digest,
                          old_digest, old_dest_digest) {
  out_of_date <- current_digest != old_digest |
    current_dest_digest !=  old_dest_digest
  out_of_date <- tidyr::replace_na(out_of_date, TRUE)
  out_of_date
}

#' Figure out which files need to be rebuilt
#'
#' \code{pdfs_to_rebuild} returns a vector of files that need to be rebuilt.
#'
#' This function accepts a vector of source files and
#' returns a vector of files that need to be rebuilt because the source file is
#' new or has changed since the last time the site was built.
#'
#' @param files A character vector of paths to source files (e.g., \code{.Rmd}).
#' @return A character vector of files that need to be rebuilt.
#' @seealso \code{\link{get_current_pdf_digests}()}, \code{\link{digests}}.
#' @keywords internal
pdfs_to_rebuild <- function(files, root_dir, static_path = "static",
                             content_path = "content") {
  root_dir <- root_dir %>% normalizePath(winslash = "/")
  files <- files %>% normalizePath(winslash = "/") %>%  unique() %>%
    purrr::keep(file.exists)



  df <- get_current_pdf_digests(files)

  df$rebuild = pdf_needs_rebuild(df$cur_digest, df$cur_dest_digest,
                             df$digest, df$dest_digest)
  df %>% dplyr::filter(rebuild) %$% file
}


#' Update all files that are out of date
#'
#' \code{update_pdfs} rebuilds all source files that are new or have changed
#' since the last time the site was built.
#'
#' Given a source directory (by default the "content" directory in the
#' root directory of the project), find all source files (\code{.Rmd} and
#' \code{.rmarkdown}) in the directory tree under the source directory,
#' calculate hashed digests of the files, and compare them to a
#' stored list of digests from the last time the site was built.
#'
#' If the digests of either the source or output files don't match,
#' if a source file is new since the last time the site was built,
#' or if the output file does not exist,
#' then render the source file.
#'
#' After rendering any out-of-date files, regenerate the digest list
#' and saves it to a file.
#'
#' @param dir A string containing the root directory for checking.
#' By default, the "content" directory of the project.
#' @param quiet Suppress output. By default this is \code{FALSE} and the
#' function emits an informational message about how many files will
#' be rebuilt.
#' @param force Force rebuilding source files that are not out of date.
#' @param method Different methods to build a website (each with pros and cons).
#'     See \code{\link[blogdown]{build_site}()} for details.
#'
#' @inheritParams blogdown::build_site
#'
#' @return This function does not return anything
#'
#' @seealso \code{\link[blogdown]{build_site}()}, \code{\link[blogdown]{build_dir}()},
#' \code{\link{digests}}.
#'
#' @export
update_pdfs <-  function(dir = NULL, root_dir = NULL,
                         static_path = "static", content_path = "content",
                         quiet = FALSE, force = FALSE) {
  if (is.null(root_dir)) {
    root_dir <- find_root_dir(dir, use_globals = TRUE)
  }
  old_wd <-  getwd()
  setwd(root_dir)
  on.exit(setwd(old_wd))
  if (is.null(dir)) {
    dir <-  content_path
  }

  cd <-  paste0(normalizePath(getwd(), winslash = "/"), "/")
  dir <- normalizePath(dir, winslash = "/")
  dir <- stringr::str_replace(dir, stringr::fixed(cd), "")
  # message("Dir = ", dir, ", cd = ", cd, ", d = ", d)

  method <- get_pdf_digest_algorithm()
  files <- find_assignment_rmds(root_dir, content_path)
  if (force) {
    to_build <- files
  } else {
    to_build <- pdfs_to_rebuild(files, root_dir, static_path, content_path)
  }
  to_build <- normalizePath(to_build, winslash = "/") %>%
    stringr::str_replace(stringr::fixed(cd), "")
  # message("To build: ", stringr::str_c(to_build, collapse = ", "))

  if (! quiet) {
    message("Building ", length(to_build), " out of date ",
            ifelse(length(to_build) == 1, "file", "files"),
            "; site has ", length(files), " ",
            ifelse(length(files) == 1, "file", "files"),
            " in total.")
  }

  for (f in to_build) {
    build_pdf_from_rmd(f, root_dir, static_path, force_dest = TRUE)
    update_pdf_file_digests(f, root_dir, static_path, content_path,
                            partial = TRUE)
  }

  # message("On exit stack: ", deparse(sys.on.exit()))
  update_pdf_file_digests(files, root_dir, static_path, content_path)
  invisible(to_build)
}

#' Rebuild changed files in a subdirectory of "content"
#'
#' `update_dir` updates changed files in a subdirectory of "content"
#'
#' @param dir A string containing the root directory for checking.
#' By default, the "content" directory of the project.
#' @param root_dir The root directory for the project (Should be the root for
#'   a HUGO project).
#' @param static_path Where to look for static files (in the HUGO sense of
#'   static).
#' @param content_path Where to look for content (e.g., `.Rmd` and `.md` files).
#' @param force Force rebuilding source files that are not out of date.
#' @param ignore A regular expression pattern for files to ignore.
#'
#' @return
#'
#' @export
update_pdf_dir <- function(dir = '.', root_dir = NULL, static_path = "static",
                       content_path = "content", force = FALSE, ignore = NA) {
  if (is.null(root_dir)) {
    start <- "."
    if (dir.exists(dir)) {
      start <- dir
    }
    root_dir <- find_root_dir(start, use_globals = TRUE)
  }
  if (! dir.exists(dir)) {
    new_dir <- file.path(root_dir, dir)
    if (! dir.exists(dir)) {
      new_dir <- file.path(root_dir, content_path, dir)
      if (! dir.exists(new_dir)) {
        stop("Directory does not exist: ", dir)
      }
    }
  }
  dir <- new_dir

  if (! is.na(ignore))
    files <- files %>% purrr::discard(~stringr::str_detect(.x, ignore))

  files <- find_assignment_rmds(root_dir, content_path, targets = dir)
  if (force) {
    to_build <- files
  } else {
    to_build <- pdfs_to_rebuild(files, root_dir, static_path, content_path)
  }
  to_build <- normalizePath(to_build, winslash = "/") %>%
    stringr::str_replace(stringr::fixed(cd), "")
  # message("To build: ", stringr::str_c(to_build, collapse = ", "))

  if (! quiet) {
    message("Building ", length(to_build), " out of date ",
            ifelse(length(to_build) == 1, "file", "files"),
            "; site has ", length(files), " ",
            ifelse(length(files) == 1, "file", "files"),
            " in total.")
  }

  for (f in to_build) {
    build_pdf_from_rmd(f, root_dir, static_path, force_dest = TRUE)
  }

  # message("On exit stack: ", deparse(sys.on.exit()))
  update_pdf_file_digests(files, root_dir, staatic_path, content_path)
  invisible(to_build)
}


#' Create a data frame with stored digests and digests of current files
#'
#' \code{get_current_pdf_digests} returns a data frame with a row for every file
#' and columns for stored and current digests of source and output files.
#'
#' This function accepts a vector of source files and
#' returns a data frame with a row for each file and columns for the
#' stored digests and the digests of current source and output files.
#'
#' @param files A character vector of paths to source files (e.g., \code{.Rmd}).
#' @return A a data frame with a row for every file and columns:
#' \describe{
#' \item{\code{file}}{The source file name.}
#' \item{\code{dest}}{The output file name.}
#' \item{\code{alg}}{The digest algorithm.}
#' \item{\code{digest}}{The stored digest for the source file.}
#' \item{\code{dest_digest}}{The stored digest for the output file.}
#' \item{\code{cur_digest}}{The digest for the current source file.}
#' \item{\code{cur_dest_digest}}{The digest for the current output file.}
#' }
#'
#' Digests for missing files are set to \code{NA}.
#' @seealso \code{\link{pdfs_to_rebuild}()},
#' \code{\link{pdf_digest_if_exists}()}, \code{\link{digests}}.
#' @keywords internal
get_current_pdf_digests <- function(files, root_dir = NULL, static_path = "static",
                                    content_path = "content") {
  if (is.null(root_dir)) {
    root_dir <- find_root_dir(dir, use_globals = TRUE)
  }
  root_dir <- root_dir %>% normalizePath(winslash = "/")

  files <- files %>% purrr::discard(is.na) %>%
    normalizePath(winslash = "/") %>%
    unique() %>% purrr::keep(file.exists)

  dest_urls <- files %>% get_pdf_url()
  df <- tibble::tibble( file = files,
                             dest = dest_urls) %>%
    dplyr::filter(! is.na(dest)) %>%
    dplyr::mutate(
      dest = purrr::map_chr(dest, ~pdf_filename(.x, root_dir = root_dir,
                                                static_path = static_path))
      )

  digest_file <- file.path(root_dir, "pdf_digests.Rds")

  if (file.exists(digest_file)) {
    digests <- readr::read_rds(digest_file) %>%
      dplyr::mutate(file = stringr::str_replace(file, "^~", root_dir)) %>%
      # Don't store the name of the output file because we're going to
      # merge digest with df by source file path, and df already has a dest
      # column.
      dplyr::select(-dest)

    # left join: we only want to check digests for the specified files.
    df <- dplyr::left_join(df, digests, by = "file")
  } else {
    # If there isn't a digest file, then the site has not been updated
    # previously, so we store NA's and build the whole site.
    df <- df %>% dplyr::mutate(
      digest = NA_character_,
      dest_digest = NA_character_,
      alg = NA_character_)
  }

  df <- df %>%
    dplyr::mutate(
      cur_dgst_lst = purrr::map2(file, alg, ~pdf_digest_if_exists(.x, .y)),
      alg = purrr::map_chr(cur_dgst_lst, ~.x['alg']),
      cur_digest = purrr::map_chr(cur_dgst_lst, ~.x['digest']),
      cur_dest_digest = purrr::map2_chr(dest, alg,
                                        ~pdf_digest_if_exists(.x, .y)['digest'])
      ) %>%
    dplyr::select(-cur_dgst_lst)

  # Organize columns in an aesthetically pleasing order.
  df <- df %>% dplyr::select(file, dest, alg, digest, dest_digest,
                            cur_digest, cur_dest_digest)
  invisible(df)
}


#' Generates and stores digests for all source and output files.
#'
#' \code{update_site_digests} calculates hashed digests for a site.
#'
#' Generates new hashed digests for both source and destination (output) files
#' and save the digests to a file "\code{digests.Rds}" in the root directory of the
#' site.
#'
#' @param dir A string with the name of the directory to search
#' (by default the "content" directory at the top-level directory of the site)
#' @param partial Logical. If \code{TRUE}, keep digests for source
#' files that aren't in the specified directory and its children and
#' descendants.
#' Otherwise, get rid of the old digest file and only keep digests for
#' source files in the source directory and its descendants.
#' @return The path to the digest file.
#' @seealso \code{\link{prune_pdf_digests}()}, \code{\link{update_site}()},
#' \code{\link{digests}}.
#' @export
#'
update_pdf_digests <- function(dir = NULL, root_dir = NULL,
                               static_path = "static", content_path = "content",
                               partial = FALSE) {
  find_assignment_rmds(root_dir, content_path, targets = dir) %>%
    update_pdf_file_digests(root_dir = root_dir, static_path = static_path,
                       content_path = content_path, partial = partial) %>%
    invisible()
}

#' Delete stored digests for specified source files
#'
#' \code{prune_pdf_digests} removes the lines from the digest file
#' corresponding to a vector of source files.
#'
#' Modifies the stored digest file to remove lines corresponding to selected
#' source files.
#'
#' @param files A character vector of paths to the source files to be removed.
#' @return The path to the digest file.
#' @seealso \code{\link{update_site_digests}()}, \code{\link{digests}}.
#' @export
#'
prune_pdf_digests <- function(files, root_dir = NULL) {
  if (is.null(root_dir)) {
    root_dir <- find_root_dir(use_globals = TRUE)
  }
  root_dir <-  root_dir
  files <-  files %>% normalizePath(winslash = "/") %>% unique() %>%
    stringr::str_replace(stringr::fixed(root_dir), "~")

  digest_file <- file.path(root_dir, "pdf_digests.Rds")

  if (length(files) && file.exists(digest_file)) {
    digests <- readr::read_rds(digest_file) %>%
      dplyr::filter(! file %in% files)
    readr::write_rds(digests, digest_file)
  }

  invisible(digest_file)
}
