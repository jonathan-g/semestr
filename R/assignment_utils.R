#' Expose contents of an environment in the current environment
#'
#' Expose the contents of an environment in the current environment.
#' Similar to [attach], but it exposes the contents only in the
#' current environment and does not change the global search.
#'
#' @param env The environment to attach locally.
#'
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

type2col <- function(type, metadata) {
  bad_idx <- bad_indices(type, metadata$type2col)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for col (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$type2col[type]
}

type2idx <- function(type, metadata) {
  bad_idx <- bad_indices(type, metadata$type2idx)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$type2idx[type]
}

type2prefix <- function(type, metadata) {
  bad_idx <- bad_indices(type, metadata$prefixes)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$prefixes[type]
}

type2base <- function(type, metadata) {
  bad_idx <- bad_indices(type, metadata$bases)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$bases[type]
}

idx2col <- function(idx, metadata) {
  bad_idx <- bad_indices(idx, metadata$idx2col)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$idx2col[idx]
}

idx2type <- function(idx, metadata) {
  bad_idx <- bad_indices(idx, metadata$idx2type)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$idx2type[idx]
}

col2idx <- function(col, metadata) {
  bad_idx <- bad_indices(col, metadata$col2idx)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$col2idx[col]
}

col2type <- function(col, metadata) {
  bad_idx <- bad_indices(col, metadata$col2type)
  assertthat::assert_that(
    length(bad_idx) == 0,
    msg = stringr::str_c("Bad type indices for idx (",
                         stringr::str_c(bad_idx, collapse = ", "), ")."))
  metadata$col2type[col]
}

base2type <- function(base, metadata) {
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
item_type <- function(cal_id, metadata) {
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
item_mod <- function(cal_id, metadata) {
  base_mod <- as.integer(cal_id) %/% mod(1000) %>%
    divide_by_int(100) %>% multiply_by(100) %>% as.character()
  metadata$rev_mod[base_mod]
}

#' Strip prefixes off keys.
#'
#' Keys in the master calendar's `topic_key` column have prefixes according to
#' the type of calennder entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function strips those off.
#'
#' @param x The list or vector to process.
#' @param metadata A list containing metadata for the differennt types.
#' @param type The type of calendar entry to strip (e.g., "`class`", "`lab`",
#' etc.)
#'
#' @return A list or vector frame with the prefixes stripped from the contents.
#'
#' @seealso add_key_prefix
#'

strip_key_prefix <- function(x, metadata, type, ...) {
  UseMethod("strip_key_prefix", x)
}

strip_key_prefix.default <- function(x, metadata, type, ...) {
  stop("I don't know how to strip key prefix from an object of class (",
       stringr::str_c(class(x), collapse = ", "), ").")
}

strip_key_prefix.character <- function(x, metadata, type, ...) {
  target <- stringr::str_c("^", metadata$prefixes[type], "_")
  x <- purrr::map_chr(x, stringr::str_replace_all, target, "")

  invisible(x)
}

strip_key_prefix.list <- function(x, metadata, type, ...) {
  x <- purrr::map(x, ~strip_key_prefix(.x, metadata, type))
  invisible(x)
}


#' Strip prefixes off keys.
#'
#' Keys in the master calendar's `topic_key` column have prefixes according to
#' the type of calennder entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function strips those off.
#'
#' @param df The data frame to process.
#' @param metadata A list containing metadata for the differennt types.
#' @param type The type of calendar entry to strip (e.g., "`class`", "`lab`",
#' etc.)
#' @param col The column where the keys are located (by default "`topic_key`").
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso add_key_prefix
#'
strip_key_prefix.data.frame <- function(df, metadata, type, col = "topic_key",
                                        ...) {
  col <- ensym(col)
  col <- enquo(col)

  df <- df %>% dplyr::mutate(!!col := strip_key_prefix(!!col, metadata, type))

  invisible(df)
}



#' Add prefixes to keys.
#'
#' Keys in the master calendar's `topic_key` column have prefixes according to
#' the type of calennder entry they represent (e.g., "`LAB_`" for labs,
#' "`CLS_`" for classes/reading assignments, "`EXAM_`" for exams, etc.). This
#' function adds those prefixes.
#'
#' @param df The data frame to process.
#' @param metadata A list containing metadata for the differennt types.
#' @param type The type of prefix to add (e.g., "`class`", "`lab`",
#' etc.)
#' @param col The column where the keys are located (by default "`topic_key`").
#'
#' @return A data frame with the prefixes stripped from the specified column.
#'
#' @seealso strip_key_prefix
#'
add_key_prefix <- function(x, metadata, type, ...) {
  UseMethod("add_key_prefix", x)
}

add_key_prefix.default <- function(x, metadata, type, ...) {
  stop("I don't know how to add key prefix to an object of class (",
       stringr::str_c(class(x), collapse = ", "), ").")
}

add_key_prefix.character <- function(x, metadata, type, ...) {
  prefix <- metadata$prefixes[type]
  x <- purrr::map_chr(x, ~stringr::str_c(prefix, .x, sep = "_"))

  invisible(x)
}

add_key_prefix.list <- function(x, metadata, type, ...) {
  x <- purrr::map(x, ~add_key_prefix(.x, metadata, type))
  invisible(x)
}



add_key_prefix.data.frame <- function(df, metadata, type, col = "topic_key") {
  col <- ensym(col)
  col <- enquo(col)

  df <- df %>% dplyr::mutate(!!col := add_key_prefix(!!col, metadata, type))
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

adj_nl <- append_newline_if_needed

concat_with_nl <- function(s, ..., start_par = FALSE, extra_lines = 0,
                           collapse = TRUE) {
  dots <- list(...)
  s <- append_newline_if_needed(s, FALSE, 0, TRUE)
  s2 <- append_newline_if_needed(as.character(unlist(dots)),
                                 start_par, extra_lines, collapse)
  if (stringr::str_starts(s2, stringr::fixed("\n")) &&
      stringr::str_ends(s, stringr::fixed("\n\n"))) {
    s <- s %>% stringr::str_replace_all("\n+$", "\n")
  }
  stringr::str_c(s, s2)
}

cat_nl <- concat_with_nl

escape_dollar <- function(txt) {
  stringr::str_replace_all(txt, c("([^\\\\])\\\\\\$" = "\\1\\\\\\\\$",
                                  "^\\\\\\$" = "\\\\\\\\$"))
}

format_month <- function(d, abbr = TRUE) {
  m <- lubridate::month(d, label = TRUE, abbr = abbr)
  if (abbr) m <- stringr::str_c(m, '.')
  m
}

format_wday <- function(d, abbr = TRUE) {
  wd <- lubridate::wday(d, label = TRUE, abbr = abbr)
  if (abbr) wd <- stringr::str_c(wd, '.')
  wd
}

format_class_date <- function(d, abbr = TRUE) {
  stringr::str_c(format_month(d, abbr = abbr), " ", lubridate::day(d))
}

format_class_day_date <- function(d, abbr_month = TRUE, abbr_wday = TRUE) {
  stringr::str_c(format_wday(d, abbr_wday), ", ",
         format_month(d, abbr_month), " ", lubridate::day(d))
}

format_date_range <- function(dates, abbr = TRUE) {
  with(dates, {
       output <- format_class_date(start, abbr)
       if (start != stop) {
       output <- stringr::str_c(output, '--',
                      ifelse(lubridate::month(stop) == lubridate::month(start),
                             lubridate::day(stop),
                             format_class_date(stop, abbr)))
       }
  })
}

format_day_date_range<- function(dates, abbr_month = TRUE, abbr_wday = TRUE) {
  with(dates, {
    output <- format_class_day_date(start, abbr_month, abbr_wday)
    if (start != stop) {
      output <- stringr::str_c(output, '--',
                               format_class_day_date(stop, abbr_month,
                                                     abbr_wday))
    }
  })
}

format_date_range_by_class_no <- function(calendar, classes, abbr = TRUE) {
  dates <- calendar %>% dplyr::filter(class %in% na.omit(classes)) %>%
    dplyr::summarize(start = min(date, na.rm = T), stop = max(date, na.rm = T))
  format_date_range(dates, abbr)
}

format_date_range_by_topic_id <- function(calendar, topics, abbr = TRUE) {
  dbg_checkpoint(g_calendar, calendar)
  dbg_checkpoint(g_topics, topics)
  dates <- calendar %>% dplyr::filter(topic_id %in% topics) %>%
    dplyr::summarize(start = min(date, na.rm = T), stop = max(date, na.rm = T))
  format_date_range(dates, abbr)
}

format_date_range_by_event_id <- function(calendar, event_ids, abbr = TRUE) {
  dates <- calendar %>% dplyr::filter(event_id %in% event_ids) %>%
    dplyr::select(date) %>% dplyr::summarize(start = min(date, na.rm = T),
                                             stop = max(date, na.rm = T))
  format_date_range(dates, abbr)
}

format_day_date_range_by_event_id <- function(calendar, event_ids,
                                              abbr_month = TRUE,
                                              abbr_wday = TRUE) {
  dates <- calendar %>% dplyr::filter(event_id %in% event_ids) %>%
    dplyr::select(date) %>% dplyr::summarize(start = min(date, na.rm = T),
                                             stop = max(date, na.rm = T))
  format_day_date_range(dates, abbr_month, abbr_wday)
}

format_page_range <- function(pages) {
  str <- stringr::str_trim(pages) %>% stringr::str_replace_all("^p+\\. *", "")
  multiple <- stringr::str_detect(pages, "-+|,|;| and ")
  stringr::str_c(ifelse(multiple, "pp. ", "p. "), pages)
}

add_period <- function(str) {
  stringr::str_trim(str, "right") %>%
    stringr::str_replace("([^.?!])$", "\\1.")
}

item_format <- function(str, item_text, pad_len) {
  lines <- stringr::str_split(str, "\n") %>% purrr::simplify()
  pad_text <- stringr::str_dup(" ", stringr::str_length(item_text))
  left_pad <- c(item_text, rep(pad_text, length(lines) - 1))
  output <- stringr::str_c(stringr::str_dup(" ", pad_len), left_pad, " ", lines) %>%
    stringr::str_trim("right") %>% stringr::str_c(collapse = "\n") %>%
    stringr::str_trim("right")
  if (stringr::str_detect(output, "\n\n")) {
    output <- stringr::str_c(output, "\n")
  }
  output
}

add_level <- function(pad_len = 0,
                      list_type = c("itemize", "enumerate", "definition"),
                      enum_type = "#.") {
  list_type = match.arg(list_type)
  if (list_type == "itemize") {
    pad_len <- pad_len + 2
  } else if (list_type == "enumerate") {
    pad_len <- pad_len + stringr::str_length(enum_type) + 1
  } else if (list_type == "definition") {
    pad_len <- pad_len + 4
  } else {
    stop("Illegal list type: ", list_type)
  }
  pad_len
}

itemize <- function(text, pad_len = 0) {
  purrr::map_chr(text, ~item_format(.x, "*", pad_len)) %>%
    stringr::str_c(collapse = "\n")
}


enumerate <- function(text, pad_len = 0, enum_type = "#.") {
  purrr::map_chr(text, ~item_format(.x, enum_type, pad_len)) %>%
    stringr::str_c(collapse = "\n")
}

expand_codes <- function(text, context, semester, delim = c("<%", "%>"), envir = NULL) {
  unlock_list <- list()
  if (is.null(envir)) {
    envir <- new.env(parent = emptyenv())
    for (sym in c("calendar", "semester_dates", "metadata")) {
      assign(sym, get(sym, envir = .globals), envir = envir)
      lockBinding(sym, envir)
    }
    assign("context", context, envir = envir)
    lockBinding("context", envir)
  } else {
    for (sym in ls(envir = envir)) {
      if (! bindingIsLocked(sym, envir)) {
        unlock_list <- c(unlock_list, sym)
        lockBinding(sym, envir)
      }
      if (exists("context", envir = envir)) {
        unlockBinding("context", envir)
      }
      assign("context", context, envir = envir)
      lockBinding("context", envir)
    }
  }

  text_codes <- semester$text_codes$md
  rlang::exec(knitr::knit_expand, !!!text_codes, text = text, delim = delim,
              .env = envir)
  for (sym in unlock_list) {
    unlockBinding(sym, envir)
  }
}

expand_code <- function(text, context, semester) {
  stringr::str_c("<%", text, "%>") %>% expand_codes(context, semester)
}

merge_dates <- function(df, semester, id_col = "cal_id", date_col = "date", ...) {
  qid_col  <- ensym(id_col)
  qid_col  <- enquo(qid_col)
  date_col <- ensym(date_col)
  date_col <- enquo(date_col)
  dots <- enquos(...)

  df <- df %>% dplyr::left_join( dplyr::select(semester$calendar,
                                               !!qid_col := cal_id,
                                               !!date_col := date,
                                               !!!dots),
                                 by = id_col)
}

make_pub_date <- function(first_date, tz = NULL) {
  if (is.null(tz)) {
    tz = get_semestr_tz()
  }
  pub_date <- first_date %>% lubridate::as_date(tz = tz) %>%
    lubridate::rollback()
  if (lubridate::today() < pub_date) pub_date <- lubridate::today()
  pub_date
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
  ext_df <- tibble::tibble(str = exts,
                           bare = stringr::str_replace_all(str, "^[+\\-]", ""),
                           num = seq_along(str))
  ext_df <- ext_df %>% dplyr::group_by(bare) %>% dplyr::top_n(1, wt = num) %>%
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

get_semestr_tz <- function(metadata = NULL) {
  if (is.null(metadata)) {
    tz <- getOption("semestr.tz")
    if (is.null(tz) || is.na(tz)) {
      tz <- "America/Chicago"
    }
  } else {
    tz <- metadata$tz
  }
  tz
}

make_root_criteria <- function(crit, ... ) {
  dots <- list(...)
  crit <- rprojroot::as.root_criterion(crit)
  for (c in dots) {
    crit <- crit | rprojroot::as.root_criterion(c)
  }
  crit
}

dbg_checkpoint <- function(tag, value) {
  qtag <- ensym(tag)
  qtag <- enquo(qtag)
  if (exists("semestr.debug") && semestr.debug ) {
    assign(as_label(qtag), value, envir = global_env())
  }
}


make_reading_context <- function(asgt, semester) {
  context <- list(
    type = "class",
    key = asgt$rd_key,
    cal_id = asgt$cal_id
    date = asgt$date,
    class_num = asgt$class_numm,
    title = asgt$topic,
  )
  context
}

make_hw_context <- function(asgt, semester) {
  context <- list(
    type = "homework",
    key = asgt$hw_key,
    cal_id = asgt$cal_id,
    due_cal_id = asgt$due_cal_id,
    date = asgt$date,
    due_date = asgt$due_date,
    title = asgt$title
  )
  context
}

make_hw_sol_context <- function(asgt, semester) {
  sol <- asgt
  asgt <- get_hw_assignment(sol$hw_key, semester)
  context <- list(
    type = "homework",
    key = asgt$hw_key,
    cal_id = asgt$cal_id,
    due_cal_id = asgt$due_cal_id,
    date = asgt$date,
    due_date = asgt$due_date,
    sol_title = sol$sol_title,
    title = asgt$title,
    sol_pub_cal_id = sol$sol_pub_cal_id,
    sol_pub_date <- dplyr::select(semester$calendar)
  )
  context

}

make_lab_context <- function(asgt, semester) {

}

make_lab_sol_context <- function(asgt, semester) {

}

make_lab_doc_context <- function(asgt, semester) {

}

make_exam_context <- function(asgt, semester) {

}


make_context <- function(asgt, type, semester) {
  dbg_checkpoint("g_context_asgt", asgt)
  dbg_checkpoint("g_context_type", type)
  if (type == "reading") {
    return(make_reading_context(asgt, semester))
  } else if (type == "lab") {
    return(make_lab_context(asgt, semester))
  } else if (type == "lab solution") {
    return(make_lab_sol_context(asgt, semester))
  } else if (type == "homework") {
    return(make_hw_context(asgt, semester))
  } else if (type == "homework solution") {
    return(make_hw_sol_context(asgt, semester))
  } else if (type == "exam") {
    return(make_exam_context(asgt, semester))
  } else {
    stop("Unknown context type ", type)
  }

}