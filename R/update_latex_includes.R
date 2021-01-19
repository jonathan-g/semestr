#' Update LaTeX style files
#'
#' Update LaTeX style files in content folders, copying from planning folder.
#'
#' @param root_dir The root directory for the project (Should be the root for
#'   a HUGO project).
#' @param planning_path Where to look for planning files (typically, in the
#'   same directory where the semester database is stored.).
#' @param content_path Where to look for content (e.g., `.Rmd` and `.md` files).
#'
#' @return NULL
#'
#' @export
update_latex_styles <- function(root_dir = NULL, content_path = "content",
                                planning_path = "planning") {
  if (is.null(root_dir)) {
    root_dir <- find_root_dir(use_globals = TRUE)
  }
  root_dir <- normalizePath(root_dir, winslash = "/")
  src_files <- list.files(file.path(root_dir, planning_path, "latex_includes"),
                          pattern = "\\.(tex|sty)$", full.names = TRUE) %>%
    normalizePath(winslash = "/")
  if (length(src_files) == 0)
    return(invisible(NULL))

  content_dirs <- assignment_source_dirs(root_dir, content_path)

  base_pat <- stringr::fixed(root_dir)

  for (src in src_files) {
    for (dest in content_dirs) {
      message("Copying ", stringr::str_replace(src, base_pat, ""),
              " to ", stringr::str_replace(dest, base_pat, ""))
      file.copy(src, dest, overwrite = TRUE, recursive = FALSE,
                copy.mode = TRUE, copy.date = TRUE)
    }
  }
  invisible(NULL)
}
