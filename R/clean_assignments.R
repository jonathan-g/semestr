#' Clean up assignment riles.
#'
#' Clean up assignment files to prepare for freshly regenerating assignments.
#'
#' @param verbose Send progress messages to the console
#' @param dry_run Simulate the cleaning without actually deleting files.
#'
#' @return NULL
#'
#' @export
clean_assignment_files <- function(verbose = TRUE, dry_run = TRUE) {

  old_wd = getwd()
  setwd(rprojroot::find_root(".semestr.here"))

  gen_dirs <- c("assignment", "homework_solutions", "lab_docs", "lab_solutions",
                "labs", "reading")
  for (d in gen_dirs) {
    fp <- file.path("content", d)
    files <- list.files(fp, pattern = ".*\\.Rmd$", full.names = TRUE)
    if (length(files) > 0) {
      if (dry_run) {
        message("would delete ", stringr::str_c(files, collapse = ", "))
      } else {
        if (verbose) {
          message("deleting ", stringr::str_c(files, collapse = ", "))
        }
        file.remove(files)
      }
    } else {
      if (verbose) {
        message("nothing to delete in ", fp)
      }
    }
  }

  static_pdf_dirs <- c("lab_docs")

  for (d in static_pdf_dirs) {
    fp <- file.path("static", "files", d)
    files <- list.files(fp, pattern = "\\.pdf$",
                        full.names = TRUE, recursive = TRUE)
    if (length(files) > 0) {
      if (dry_run) {
        message("would delete ", stringr::str_c(files, collapse = ", "))
      } else {
        if (verbose) {
          message("deleting ", stringr::str_c(files, collapse = ", "))
        }
        file.remove("files")
      }
    } else {
      if (verbose) {
        message("nothing to delete in ", fp)
      }
    }
  }

  static_img_dirs <- c("lab_docs")

  for (d in static_img_dirs) {
    fp <- file.path("static", d)
    files <- list.files(fp, pattern = "\\.(png|jpg|jpeg|svg|gif|pdf)$",
                        full.names = TRUE, recursive = TRUE)
    if (length(files) > 0) {
      if (dry_run) {
        message("would delete ", stringr::str_c(files, collapse = ", "))
      } else {
        if (verbose) {
          message("deleting ", stringr::str_c(files, collapse = ", "))
        }
        file.remove("files")
      }
    } else {
      if (verbose) {
        message("nothing to delete in ", fp)
      }
    }
  }

  setwd(old_wd)
  NULL
}
