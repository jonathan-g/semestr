any_true <- function(vec) {
  any(purrr::map_lgl(vec, isTRUE))
}

append_newline_if_needed <- function(txt, start_par = FALSE, extra_lines = 0) {
  txt <- stringr::str_trim(txt)
  txt[stringr::str_detect(txt, '[^\n]$')] <- stringr::str_c(txt, '\n')
  if (start_par)
    txt <- stringr::str_c("\n", txt)
  if (extra_lines > 0)
    txt <- stringr::str_c(txt, stringr::str_c(rep('\n', extra_lines),
                                              collapse = ''))
  txt
}

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

expand_codes <- function(text, semester, delim = c("<%", "%>")) {
  text_codes <- semester$text_codes$md
  rlang::exec(knitr::knit_expand, !!!text_codes, text = text, delim = delim)
}

expand_code <- function(text, semester) {
  stringr::str_c("<%", text, "%>") %>% expand_codes(semester)
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

pub_date <- function() {
  pub_date <- first_date %>% as_date(tz = get_semestr_tz()) %>% rollback()
  if (today() < pub_date) pub_date <- today()
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

get_semestr_tz <- function() {
  tz <- getOption("semestr.tz")
  if (is.null(tz) || is.na(tz)) {
    tz <- "America/Chicago"
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
  if (exists("semestr.debug") && semester.debug ) {
    assign(as_label(qtag), value, envir = global_env())
  }
}
