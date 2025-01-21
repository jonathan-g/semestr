#' Expose contents of an environment in the current environment
#'
#' Expose the contents of an environment in the current environment.
#' Similar to [attach], but it exposes the contents only in the
#' current environment and does not change the global search.
#'
#' @param env The environment to attach locally.
#'
#' @export
pull_env <- function(env) {
  for (n in ls(env)) {
    assign(n, get(n, envir = env), envir = parent.frame())
  }
}

bad_indices <- function(idx, target) {
  idx <- purrr::discard(idx, is.na)
  bad <- setdiff(idx, names(target))
  bad
}

set_md_extensions <- function(ext_str, append = FALSE) {
  if (append) {
    ext_str = stringr::str_c(getOption("semestr.md_extensions"), ext_str,
                             sep = "", collapse = "")
  }
  exts <- ext_str %>% stringr::str_split(" +", simplify = TRUE) %>%
    as.character() %>%
    stringr::str_extract_all("[[:space:]+\\-]+[a-zA-Z0-9_]+") %>%
    purrr::simplify() %>%
    stringr::str_trim()
  ext_df <- tibble::tibble(str = exts) %>%
    dplyr::mutate(bare = stringr::str_replace_all(.data$str, "^[+\\-]", ""),
                  num = seq(dplyr::n()))
  ext_df <- ext_df %>% dplyr::group_by(.data$bare) %>%
    dplyr::top_n(1, wt = .data$num) %>%
    dplyr::ungroup()
  ext_str <- unique(ext_df$str) %>%
    stringr::str_c(sep = "", collapse = "")
  invisible(ext_str)
}

get_md_extensions <- function() {
  exts <- getOption("semestr.md_extensions")
  if (is.null(exts)) {
    exts <- "+tex_math_single_backslash+compact_definition_lists"
  }
  exts
}


#' Create default metadata
#'
#' `default_semestr_metadata` creates a named list with metadata for the
#'   structure of a syllabus.
#'
#' @return A named list whose elements are named vectors corresponding to the
#'   type of syllabus entries, the data frame columns used to store data for
#'   those events, prefixes used in calendar keys, and numeric bases used for
#'   calendar ID entries, as well as modifiers for calendar IDs used to
#'   indicate canceled and rescheduled classes.
#'
#' @export
default_semestr_metadata <- function() {
  list(
    type2idx = c(class = "class", lab = "lab", homework = "homework",
                 "due date" = "due_date", exam = "exam", holiday = "holiday",
                 event = "event"),
    type2col = c(class = "class", lab = "lab",  homework = "hw",
                 "due date" = "due", exam = "exam", holiday = "holiday",
                 event = "evt"),
    idx2type = c(class = "class", lab = "lab", homework = "homework",
                 due_date = "due date", exam = "exam", holiday = "holiday",
                 event = "event" ),
    col2type = c(class = "class", lab = "lab", hw = "homework",
                 due = "due date", exam = "exam", holiday = "holiday",
                 evt = "event" ),
    idx2col = c(class = "class", lab = "lab", homework = "hw",
                due_date = "due", exam = "exam", holiday = "holiday",
                event = "evt" ),
    col2idx = c(class = "class", lab = "lab", hw = "homework",
                due = "due_date", exam = "exam", holiday = "holiday",
                evt = "event" ),
    prefixes = c(class = "CLS", lab = "LAB", homework = "HW",
                 "due date" = "DUE", exam = "EXAM", holiday = "VAC",
                 event = "EVT" ),
    bases = c(class = 1000, homework = 2000, lab = 3000, "due date" = 4000,
              exam = 5000, holiday = 6000, event = 7000),
    rev_base = c("1000" = "class", "2000" = "homework", "3000" = "lab",
                 "4000" = "due date", "5000" = "exam",  "6000" = "holiday",
                 "7000" = "event"),
    mods = c(canceled = 100,  make_up = 200),
    rev_mods = c("100" = "canceled", "200" = "make_up" )
  )
}


#' Get the metadata for the currently loaded semester
#'
#' Pulls the metadata for the currently loaded semester from the package's
#' `.globals`  environment
#'
#' @return A named list with the metadata
#' @examples
#' get_semestr_metadata()
#'
#' @export
get_semestr_metadata <- function() {
  if (exists("metadata", envir = .globals)) {
    get("metadata", envir = .globals)
  } else {
    default_semestr_metadata()
  }
}


#' Get the time zone for where the course will be taught.
#'
#' The semester planning database should contain information about the
#' time zone where the course will be taught. This function retrieves the
#' time zone corresponding to the currently loaded semester schedule.
#'
#' @return A character string with the time zone.
#' @examples
#' get_semestr_tz()
#'
#' @export
get_semestr_tz <- function() {
  if (exists("tz", envir = .globals)) {
    tz <- .globals$tz
  } else
    tz <- getOption("semestr.tz")
  if (is.null(tz) || is.na(tz)) {
    tz <- "America/Chicago"
  }
  tz
}


strip_leading_slash <- function(s) {
  stringr::str_replace_all(s, "^[/\\\\]+", "")
}

cat_path <- function(dir, base) {
  file.path(dir, strip_leading_slash(base))
}

make_root_criteria <- function(crit, ... ) {
  dots <- list(...)
  crit <- rprojroot::as.root_criterion(crit)
  for (c in dots) {
    crit <- crit | rprojroot::as.root_criterion(c)
  }
  crit
}

find_root_dir <- function(path = ".", crit = NULL, use_globals = FALSE) {
  if (use_globals) {
    if (exists("root_dir", envir = .globals)) {
      if (dir.exists(.globals$root_dir)) {
        return (.globals$root_dir)
      }
    }
  }
  if (is.null(crit)) {
    crit <- make_root_criteria(".semestr.here",
                               rprojroot::has_file_pattern("^.*\\.RProj$"),
                               rprojroot::has_dir(".Rproj.user"),
                               rprojroot::has_dir("content"))

  } else {
    crit <- rprojroot::as.root_criterion(crit)
  }

  root_dir <- rprojroot::find_root(crit, path)
  root_dir
}

assignment_source_dirs <- function(root_dir = NULL, content_path = "content",
                                   targets = NULL) {
  if (is.null(root_dir)) {
    root_dir <- find_root_dir(use_globals = TRUE)
  }
  content_path <- strip_leading_slash(content_path)

  targets_avail <-  c("assignment", "homework_solutions", "lab_docs",
                      "lab_solutions", "labs", "reading")

  if (is.null(targets)) {
    targets <- targets_avail
  } else {
    targets <- interaction(basename(targets), targets_avail)
  }

  targets <- file.path(root_dir, content_path, targets) %>%
    purrr::keep(dir.exists)
  targets
}

find_assignment_rmds <- function(root_dir = NULL, content_path = "content",
                                 targets = NULL) {
  list.files(assignment_source_dirs(root_dir, content_path, targets),
             pattern = "\\.Rmd$", full.names = TRUE)
}



#' Extends is.na to report TRUE if the object has length zero.
#'
#' If I want to check whether an element in a list is missing, sometimes it's
#' `NA`, but sometimes it is simply a vector of length 0. This checks for both
#' possibilities.
#'
#' @param x A vector of any type, of length 0 or 1
#'
#' @return TRUE if the vector has length 0 or is NA
#' @examples
#' is_mt_or_na(character(0))
#' is_mt_or_na(NA_character_)
#' is_mt_or_na("foo")
#'
#' @export
is_mt_or_na <- function(x) {
  length(x) == 0 || any(is.na(x))
}

clean_url <- function(s) {
  stringr::str_replace_all(s, "(^|[^\\:])//+", "\\1/") %>%
    stringr::str_replace_all("(://)/+", "\\1")
}

clean_path <- function(s) {
  stringr::str_replace_all(s, "//+", "/")
}
