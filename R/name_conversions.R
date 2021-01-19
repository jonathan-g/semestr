
#' Metadata Name Conversion Functions
#'
#' Convert between different types of metadata representing categories of
#'   possible calendar entries.
#'
#' The kinds of metadata include
#' * **type**: A type of calendar entry, in text. "class", "lab", "homework",
#'   "due date", "exam", "holiday", and "event".
#' * **idx**: Indices for entries in vectors and lists: "class", "homework",
#'   "due_date", "exam", "holiday", "event", "notice".
#' * **col**: A string used as prefix or suffix for a data frame column:
#'   "class", "lab", "hw", "due", "exam", "holiday", "event", "notice".
#' * **prefix**: A prefix used to distinguish calendar keys:
#'   "CLS_", "LAB_", "HW_", "DUE_", "EXAM_", "VAC_", and "EVT_".
#' * **base**: An integer value used to identify numeric calendar entries.
#'   Multiples of 1000.
#'
#' @param type A type (character)
#' @param idx An index (character)
#' @param col A column prefix/suffix (character)
#' @param prefix A calendar key prefix (character)
#' @param base An integer base.#'
#'
#' @return A character or integer value corresponding to the request.
#'
#' @name name_conversions
NULL

#' @describeIn name_conversions Convert a type to a column name.
#'
#' @export
type2col <- function(type) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(type, metadata$type2col)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for col (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$type2col[type]
}


#' @describeIn name_conversions Convert a type to an index.
#'
#' @export
type2idx <- function(type) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(type, metadata$type2idx)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$type2idx[type]
}


#' @describeIn name_conversions Convert a type to a prefix.
#'
#' @export
type2prefix <- function(type) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(type, metadata$prefixes)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$prefixes[type]
}


#' @describeIn name_conversions Convert a type to a base.
#'
#' @export
type2base <- function(type) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(type, metadata$bases)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$bases[type]
}

#' @describeIn name_conversions Convert an index to a column.
#'
#' @export
idx2col <- function(idx) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(idx, metadata$idx2col)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$idx2col[idx]
}


#' @describeIn name_conversions Convert an index to a type.
#'
#' @export
idx2type <- function(idx) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(idx, metadata$idx2type)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$idx2type[idx]
}


#' @describeIn name_conversions Convert a column to an index.
#'
#' @export
col2idx <- function(col) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(col, metadata$col2idx)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$col2idx[col]
}


#' @describeIn name_conversions Convert a column to a type.
#'
#' @export
col2type <- function(col) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(col, metadata$col2type)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$col2type[col]
}


#' @describeIn name_conversions Convert a base to a type.
#'
#' @export
base2type <- function(base) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(base, metadata$rev_base)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$rev_base[base]
}

#' Determine the type of calendar entry from its calendar id.
#'
#' @param cal_id an integer calendar ID number.
#'
#' @return A string identifying the type of calendar entry. Current values
#'   are "class", "reading", "homework", "lab", "exam", "due date", "holiday",
#'   and "event".
#'
#' @export
item_type <- function(cal_id) {
  metadata <- get_semestr_metadata()
  base <- as.integer(cal_id) %>%
    divide_by_int(1000) %>% multiply_by(1000) %>% as.character()
  metadata$rev_base[base]
}

#' Determine the modification type of calendar entry from its calendar id.
#'
#' Modifications include canceled and re-scheduled (make-up) classes.
#'
#' @param cal_id an integer calendar ID number.
#'
#' @return A string identifying the type of modification. Current values are
#'   "canceled" and "make-up"
#'
#' @export
item_mod <- function(cal_id) {
  metadata <- get_semestr_metadata()
  base_mod <- as.integer(cal_id) %/% mod(1000) %>%
    divide_by_int(100) %>% multiply_by(100) %>% as.character()
  metadata$rev_mod[base_mod]
}

