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
    bases = c(class = 1000, lab = 2000, homework = 3000, "due date" = 4000,
              exam = 5000, holiday = 6000, event = 7000),
    rev_base = c("1000" = "class",  "2000" = "lab", "3000" = "homework",
                 "4000" = "due date", "5000" = "exam",  "6000" = "holiday",
                 "7000" = "event"),
    mods = c(cancelled = 100,  make_up = 200),
    rev_mods = c("100" = "cancelled", "200" = "make_up" )
  )
}

get_semestr_metadata <- function() {
  if (exists("metadata", envir = .globals)) {
    get("metadata", envir = .globals)
  } else {
    default_semestr_metadata()
  }
}

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


