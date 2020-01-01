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
