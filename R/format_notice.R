make_notice <- function(notice_entries) {
  if (is.null(notice_entries) || nrow(notice_entries) == 0) {
    return(NULL)
  }
  if (nrow(notice_entries) > 1) {
    output <- stringr::str_c("## Notices:", "",
                             stringr::str_c("*", notice_entries$notice,
                                            sep = "  ", collapse = "\n"), "", "",
                             sep = "\n")
  } else {
    output <- stringr::str_c("## Notice:", "",
                             notice_entries$notice, "",
                             sep = "\n")
  }
  output %>% escape_dollar()
}
