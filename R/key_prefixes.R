#' Strip prefixes off keys.
#'
#' Keys in the master calendar's `cal_key` column have prefixes according to
#' the type of calendar entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function strips those off.
#'
#' @param x An object to process.
#' @param type The type of calendar entry to strip (e.g., "`class`", "`lab`",
#' etc.)
#' @param ... Arguments to pass to specialized methods.
#'
#' @return A list or vector frame with the prefixes stripped from the contents.
#'
#' @seealso add_key_prefix
#'
strip_key_prefix <- function(x, type, ...) {
  UseMethod("strip_key_prefix", x)
}

#' @exportS3Method semestr::strip_key_prefix
strip_key_prefix.default <- function(x, type, ...) {
  stop("I don't know how to strip key prefix from an object of class (",
       stringr::str_c(class(x), collapse = ", "), ").")
}

#' @describeIn strip_key_prefix Strip key prefixes from character vector.
strip_key_prefix.character <- function(x, type, ...) {
  target <- stringr::str_c("^", get_semestr_metadata()$prefixes[type], "_")
  x <- purrr::map_chr(x, stringr::str_replace_all, target, "")

  invisible(x)
}

#' @describeIn strip_key_prefix Strip key prefixes from a list of character objects.
strip_key_prefix.list <- function(x, type, ...) {
  x <- purrr::map(x, ~strip_key_prefix(.x, type))
  invisible(x)
}


#' @describeIn strip_key_prefix Strip key prefixes from a column in a data frame.
#'
#' @param x The data frame to process.
#' @param type The type of calendar entry to strip (e.g., "`class`", "`lab`",
#' etc.)
#' @param col The column where the keys are located (by default "`cal_key`").
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso add_key_prefix
#'
strip_key_prefix.data.frame <- function(x, type, col = "cal_key", ...) {
  col <- ensym(col)
  col <- enquo(col)

  x <- x %>% dplyr::mutate(!!col := strip_key_prefix(!!col, type))

  invisible(x)
}

#' Add prefixes to keys.
#'
#' Keys in the master calendar's `cal_key` column have prefixes according to
#' the type of calendar entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function adds those prefixes.
#'
#' @param x The object to process.
#' @param type The type of prefix to add (e.g., "`class`", "`lab`",
#' etc.)
#' @param ... Arguments passed to methods.
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso strip_key_prefix
#'
add_key_prefix <- function(x, type, ...) {
  UseMethod("add_key_prefix", x)
}

#' @exportS3Method semestr::add_key_prefix
add_key_prefix.default <- function(x, type, ...) {
  stop("I don't know how to add key prefix to an object of class (",
       stringr::str_c(class(x), collapse = ", "), ").")
}

#' @describeIn add_key_prefix Add key prefixes to a character vector.
add_key_prefix.character <- function(x, type, ...) {
  prefix <- get_semestr_metadata()$prefixes[type]
  x <- purrr::map_chr(x, ~stringr::str_c(prefix, .x, sep = "_"))

  invisible(x)
}

#' @describeIn add_key_prefix Add key prefixes to a list of character objects.
add_key_prefix.list <- function(x, type, ...) {
  x <- purrr::map(x, ~add_key_prefix(.x, type))
  invisible(x)
}

#' @describeIn add_key_prefix Add key prefix to a column in a data frame.
#' @param x The object to process.
#' @param type The type of prefix to add (e.g., "`class`", "`lab`",
#' etc.)
#' @param col The column to process
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso strip_key_prefix
#'
add_key_prefix.data.frame <- function(x, type, col = "cal_key", ...) {
  col <- ensym(col)
  col <- enquo(col)

  x <- x %>% dplyr::mutate(!!col := add_key_prefix(!!col, type))
  invisible(x)
}

any_true <- function(vec) {
  any(purrr::map_lgl(vec, isTRUE))
}
