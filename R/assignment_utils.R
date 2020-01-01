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


