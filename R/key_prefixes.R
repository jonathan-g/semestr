#' Strip prefixes off keys.
#'
#' Keys in the master calendar's `cal_key` column have prefixes according to
#' the type of calennder entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function strips those off.
#'
#' @param x The list or vector to process.
#' @param type The type of calendar entry to strip (e.g., "`class`", "`lab`",
#' etc.)
#'
#' @return A list or vector frame with the prefixes stripped from the contents.
#'
#' @seealso add_key_prefix
#'

strip_key_prefix <- function(x, type, ...) {
  UseMethod("strip_key_prefix", x)
}

strip_key_prefix.default <- function(x, type, ...) {
  stop("I don't know how to strip key prefix from an object of class (",
       stringr::str_c(class(x), collapse = ", "), ").")
}

strip_key_prefix.character <- function(x, type, ...) {
  target <- stringr::str_c("^", get_semestr_metadata()$prefixes[type], "_")
  x <- purrr::map_chr(x, stringr::str_replace_all, target, "")

  invisible(x)
}

strip_key_prefix.list <- function(x, type, ...) {
  x <- purrr::map(x, ~strip_key_prefix(.x, type))
  invisible(x)
}


#' Strip prefixes off keys.
#'
#' Keys in the master calendar's `cal_key` column have prefixes according to
#' the type of calennder entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function strips those off.
#'
#' @param df The data frame to process.
#' @param type The type of calendar entry to strip (e.g., "`class`", "`lab`",
#' etc.)
#' @param col The column where the keys are located (by default "`cal_key`").
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso add_key_prefix
#'
strip_key_prefix.data.frame <- function(df, type, col = "cal_key",
                                        ...) {
  col <- ensym(col)
  col <- enquo(col)

  df <- df %>% dplyr::mutate(!!col := strip_key_prefix(!!col, type))

  invisible(df)
}



#' Add prefixes to keys.
#'
#' Keys in the master calendar's `cal_key` column have prefixes according to
#' the type of calennder entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function adds those prefixes.
#'
#' @param df The data frame to process.
#' @param type The type of prefix to add (e.g., "`class`", "`lab`",
#' etc.)
#' @param col The column where the keys are located (by default "`cal_key`").
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso strip_key_prefix
#'
add_key_prefix <- function(x, type, ...) {
  UseMethod("add_key_prefix", x)
}

add_key_prefix.default <- function(x, type, ...) {
  stop("I don't know how to add key prefix to an object of class (",
       stringr::str_c(class(x), collapse = ", "), ").")
}

add_key_prefix.character <- function(x, type, ...) {
  prefix <- get_semestr_metadata()$prefixes[type]
  x <- purrr::map_chr(x, ~stringr::str_c(prefix, .x, sep = "_"))

  invisible(x)
}

add_key_prefix.list <- function(x, type, ...) {
  x <- purrr::map(x, ~add_key_prefix(.x, type))
  invisible(x)
}



add_key_prefix.data.frame <- function(df, type, col = "cal_key") {
  col <- ensym(col)
  col <- enquo(col)

  df <- df %>% dplyr::mutate(!!col := add_key_prefix(!!col, type))
  invisible(df)
}

any_true <- function(vec) {
  any(purrr::map_lgl(vec, isTRUE))
}

append_newline_if_needed <- function(txt, start_par = FALSE, extra_lines = 0,
                                     collapse = NULL) {
  txt <- stringr::str_trim(txt)
  txt[stringr::str_detect(txt, '[^\n]$')] <- stringr::str_c(txt, '\n')
  if (length(start_par) > 1) {
    assertthat::assert_that(length(txt) == length(start_par),
                            msg = stringr::str_c("txt has length ", length(txt),
                                                 " and start_par has length ",
                                                 length(start_par)))
    txt <- stringr::str_c(ifelse(start_par, "\n", ""), txt)

  } else if (start_par) {
    txt <- stringr::str_c("\n", txt)
  }
  if(length(extra_lines) > 1) {
    assertthat::assert_that(length(txt) == length(extra_lines),
                            msg = stringr::str_c("txt has length ", length(txt),
                                                 " and extra_lines has length ",
                                                 length(extra_lines)))
    txt <- stringr::str_c(txt,
                          map_chr(extra_lines,
                                  ~stringr::str_c(ifelse(.x > 0,
                                                         rep("\n", .x), ""),
                                                  collapse = "")))
  } else if (extra_lines > 0) {
    txt <- stringr::str_c(txt, stringr::str_c(rep('\n', extra_lines),
                                              collapse = ''))
  }
  if (! is.null(collapse)) {
    if (isTRUE(collapse)) {
      collapse <- ""
    }
    txt <- stringr::str_c(txt, collapse = collapse)
  }
  txt
}
