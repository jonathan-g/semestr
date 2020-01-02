type2col <- function(type) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(type, metadata$type2col)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for col (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$type2col[type]
}

type2idx <- function(type) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(type, metadata$type2idx)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$type2idx[type]
}

type2prefix <- function(type) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(type, metadata$prefixes)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$prefixes[type]
}

type2base <- function(type) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(type, metadata$bases)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$bases[type]
}

idx2col <- function(idx) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(idx, metadata$idx2col)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$idx2col[idx]
}

idx2type <- function(idx) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(idx, metadata$idx2type)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$idx2type[idx]
}

col2idx <- function(col) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(col, metadata$col2idx)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$col2idx[col]
}

col2type <- function(col) {
  metadata <- get_semestr_metadata()
  bad_idx <- bad_indices(col, metadata$col2type)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$col2type[col]
}

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
#' @param metadata A list of metadata as returned from [load_semester_db].
#'
#' @return A string identifying the type of calendar entry. Current values
#'   are "class", "reading", "homework", "lab", "exam", "due date", "holiday",
#'   and "event".
#'
item_type <- function(cal_id) {
  metadata <- get_semestr_metadata()
  base <- as.integer(cal_id) %>%
    divide_by_int(1000) %>% multiply_by(1000) %>% as.character()
  metadata$rev_base[base]
}

#' Determine the modification type of calendar entry from its calendar id.
#'
#' Modifications include cancelled and re-scheduled (make-up) classes.
#'
#' @param cal_id an integer calendar ID number.
#' @param metadata A list of metadata as returned from [load_semester_db].
#'
#' @return A string identifying the type of modification. Current values are
#'   "cancelled" and "make-up"
#'
item_mod <- function(cal_id) {
  metadata <- get_semestr_metadata()
  base_mod <- as.integer(cal_id) %/% mod(1000) %>%
    divide_by_int(100) %>% multiply_by(100) %>% as.character()
  metadata$rev_mod[base_mod]
}

