make_notice <- function(notice_entries) {
  if (length(notice_entries) > 1) {
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
