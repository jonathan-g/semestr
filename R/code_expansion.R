.expand_env <- new.env(parent = emptyenv())

.expand_env$globals <- list()

.expand_env$packages <- c("magrittr", "dplyr", "lubridate", "purrr", "stringr",
                          "tidyr")

.expand_env$imports <-
  c("add_period", "escape_dollar",
    "format_class_date", "format_class_day_date",
    "format_date_by_cal_id", "format_date_by_class_num", "format_date_by_key",
    "format_date_range", "format_date_range_by_cal_id",
    "format_date_range_by_class_num", "format_date_range_by_event_id",
    "format_date_range_by_key", "format_day_date_by_cal_id",
    "format_day_date_by_class_num", "format_day_date_by_key",
    "format_day_date_range", "format_handout_reading",
    "format_handout_reading_item", "format_month", "format_page_range",
    "format_textbook_reading", "format_textbook_reading_item", "format_wday",
    "get_semestr_metadata", "get_semestr_tz", "default_semestr_metadata",
    "type2base", "type2idx", "type2prefix", "type2col",
    "col2idx", "col2type",
    "idx2col", "idx2type",
    "base2type", "item_type", "item_mod"
    )

make_fn_body <- function(..., expr_lst = NULL) {
  if (is.null(expr_lst)) {
    # message("building expr_lst from dots")
    expr_lst <- list(...)
    # message("expr_lst has ", length(expr_lst), " exprs.")
    # } else {
    #   # message("expr_lst from argument: has ", length(expr_lst), " exprs.")
  }
  body <- c(expr(`{`), expr_lst)
  body <- as.call(body)
  body
}

unpack_fn_body <- function(x) {
  # assertthat::assert_that(is.call(x))
  if (is.call(x)) {
    x <- as.list(x)
    # assertthat::assert_that(x[[1]] == expr(`{`))
    if (x[[1]] == expr(`{`)) {
      # message("stripping first element...")
      x <- x[-1]
      # } else {
      #   # message("First element is not `{`")
    }
    # } else {
    #   # message("x is not a call.")
  }
  assertthat::assert_that(is_list(x) || is_expression(x))
  x
}

merge_fn_bodies <- function(..., body_lst = NULL) {
  if (is.null(body_lst)) {
    body_lst <- list(...)
  }
  bodies <- purrr::map(body_lst, unpack_fn_body)
  bodies <- unlist(bodies)
  # message("bodies has ", length(bodies), " exprs")
  body <- make_fn_body(expr_lst = bodies)
  body
}

# build_pkg_handlers <- function(n_levels = 1) {
#   packages <- .GlobalEnv$.globals$expand_packages
#
#   .local_envir = expr(rlang::call_frame(n = !!n_levels)$env)
#
#   att <- purrr::map(packages, function(x) {
#     q <- ensym(x) %>% as_label()
#     p <- stringr::str_c("package:", q)
#     list(
#       expr(if (! (!!q) %in% .packages()) {
#         attachNamespace(!!q)
#         # withr::defer(detach(!!p, character.only = TRUE),
#         #              envir = !!.local_envir)
#       })
#     )
#   }) %>% unlist()
#   att <- make_fn_body(expr_lst = att)
#
#   det <- purrr::map(packages, function(x) {
#     q <- ensym(x) %>% as_label()
#     p <- stringr::str_c("package:", q)
#     list(
#       expr(detach(!!p, character.only = TRUE))
#     )
#   }) %>% flatten()
#   det <- make_fn_body(expr_lst = det)
#
#   list(attach = att, detach = det)
# }
#

expand_codes <- function(text, context, semester, delim = c("<%", "%>"),
                         envir = NULL, extra_packages = NULL) {
  dbg_checkpoint(g_expansion_text, text)
  dbg_checkpoint(g_expansion_context, context)

  loaded <- .packages()

  if (exists("packages", envir = .expand_env)) {
    for (p in setdiff(.expand_env$packages, loaded)) {
      withr::local_package(p, character.only = TRUE)
    }
  }

  loaded2 <- .packages()

  if (! is.null(extra_packages)) {
    for (p in setdiff(extra_packages, loaded2)) {
      withr::local_package(p, character.only = TRUE)
    }
  }

  unlock_list <- list()
  local_env <- envir
  if (is.null(local_env)) {
    local_env <- new.env(parent = as.environment(search()[2]))

    for (sym in c("calendar", "semester_dates", "metadata", "tz")) {
      assign(sym, get(sym, envir = .globals), envir = local_env)
      lockBinding(sym, local_env)
    }
    ee_globals <- get("globals", envir = .expand_env)
    for (sym in names(ee_globals)) {
      assign(sym, ee_globals[[sym]], envir = local_env)
      lockBinding(sym, local_env)
    }
    for (sym in .expand_env$imports) {
      assign(sym, get(sym, pos = 1), envir = local_env)
      lockBinding(sym, local_env)
    }
    assign("context", context, envir = local_env)
    lockBinding("context", local_env)
  } else {
    for (sym in ls(envir = local_env)) {
      if (! bindingIsLocked(sym, local_env)) {
        unlock_list <- c(unlock_list, sym)
        lockBinding(sym, local_env)
      }
      if (exists("context", envir = local_env)) {
        unlockBinding("context", local_env)
      }
      assign("context", context, envir = local_env)
      lockBinding("context", local_env)
    }
  }

  text_codes <- semester$text_codes$md

  dbg_checkpoint(g_expnaion_env, local_env)
  dbg_checkpoint(g_text_codes, text_codes)
  dbg_checkpoint(g_expansion_delims, delim)

  expand_expr <- c(
    expr(
     knitr::knit_expand( !!!text_codes, text = !!text, delim = !!delim)
    )
  )

  expand_body <- make_fn_body(expr_lst = expand_expr)

  # message("Local env contains: ",
  #         stringr::str_c(ls(local_env), collapse = ", ")
  #         )

  retval <- eval(expand_body, envir = local_env)

  for (sym in unlock_list) {
    unlockBinding(sym, local_env)
  }
  retval
}

expand_code <- function(text, context, semester) {
  stringr::str_c("<%", text, "%>") %>% expand_codes(context, semester)
}

