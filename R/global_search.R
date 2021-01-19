#' Search data frames for a pattern in any column.
#'
#' Search data frames and return lines where any column matches a pattern.
#'
#' @param df_lst A named list of data frames
#' @param pattern The pattern to search for (regular expression)
#'
#' @return A named list of filtered data frames
#'
#' @export
global_search <- function(df_lst, pattern) {
  hit_list <- list()
  for (nm in names(df_lst)) {
    df = df_lst[[nm]]
    if (is.data.frame(df)) {
      hits <- df %>%
        dplyr::filter(dplyr::across(where(is.character),
                                    ~stringr::str_detect(.x, pattern)) %>%
                        purrr::reduce(`|`))
      if (nrow(hits) > 0) {
        hit_list[[nm]] <- hits
      }
    }
  }
  hit_list
}

#' Search for a pattern in any column of a data frame
#'
#' Search every character column of a data frame for a pattern.
#'
#' @param df A data frame
#' @param pattern The pattern to search for (regular expression)
#'
#' @return A data frame where each character column is replaced with "HIT" if
#'   it matches the pattern and "" if it doesn't.
#'
#' @export
col_search <- function(df, pattern) {
  df %>% dplyr::mutate(dplyr::across(where(is.character),
                                     ~stringr::str_detect(.x, pattern)))
}

#' Count the number of times each column in a data frame matches a pattern.
#'
#' Search every character column of a data frame for a pattern and count hits.
#'
#' @param df A data frame
#' @param pattern The pattern to search for (regular expression)
#' @param na.rm Ignore missing values.
#'
#' @return A data frame where each column is the number of times that column
#'   matched the target pattern.
#'
#' @export
col_search_sum <- function(df, pattern, na.rm = TRUE) {
  df %>% dplyr::summarize(dplyr::across(where(is.character),
                                        ~sum(stringr::str_detect(., pattern),
                                             na.rm = na.rm)))
}


#' Search and replace all columns in a data frame.
#'
#' Search and replace in every column of a data frame.
#'
#' @param df A data frame
#' @param pattern The pattern to search for (regular expression)
#' @param replacement The replacement.
#'
#' @return A data frame with the pattern replaced in every column.
#'
#' @export
col_replace <- function(df, pattern, replacement) {
  df <- df %>%
    dplyr::mutate(dplyr::across(where(is.character),
                                ~stringr::str_replace_all(.x, pattern, replacement)))
  invisible(df)
}


#' Search data frames for a pattern in any column
#'
#' Search data frames and return lines where any column matches a pattern.
#'
#' @param df_lst A named list of data frames
#' @param pattern The pattern to search for (regular expression)
#'
#' @return A named list of data frames where character columns are replaced
#'   with "HIT" for matches and "" for misses.
#'
#' @export
global_col_search <- function(df_lst, pattern) {
  global_search(df_lst, pattern) %>%
    purrr::map(~col_search(.x, pattern))
}

#' Count pattern-matches in each column of data frames
#'
#' Search a list of data frames for a pattern and count the number of matches
#' in each column.
#'
#' @param df_lst A named list of data frames
#' @param pattern The pattern to search for (regular expression)
#'
#' @return A named list of data frames where each column has the number of
#'   times that column matched the pattern.
#'
#' @export
global_col_search_sum <- function(df_lst, pattern) {
  global_search(df_lst, pattern) %>%
    purrr::map(~col_search_sum(.x, pattern))
}


#' Search and replace all columns in a list of data frames.
#'
#' Search and replace in every column of a list of data frames.
#'
#' @param df_lst A named list of data frames
#' @param pattern The pattern to search for (regular expression)
#' @param replacement The replacement.
#'
#' @return A named list of data frames.
#'
#' @export
global_replace <- function(df_lst, pattern, replacement) {
  for (nm in names(df_lst)) {
    df = df_lst[[nm]]
    if (is.data.frame(df)) {
      df <- df %>%
        dplyr::mutate(dplyr::across(where(is.character),
                                    ~stringr::str_replace_all(.x, pattern, replacement)))
      df_lst[[nm]] <- df
    }
  }
  invisible(df_lst)
}
