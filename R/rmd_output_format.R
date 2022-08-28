#' Make an RMD output format
#'
#' Make an output format for creating blogdown and PDF output.
#'
#' @param toc Include a table of contents?
#' @param toc_depth Depth of the table of contents
#' @param includes list of files to include.
#'
#' @return A list containing a named list of output formats.
make_rmd_output_format <- function(toc = FALSE, toc_depth = NULL,
                                   includes = NULL) {
  output <- list(
    list(
      "blogdown::html_page" =
        list(md_extensions = get_md_extensions(),
             toc = toc),
      pdf_document =
        list(md_extensions = get_md_extensions(), toc = toc,
             toc_depth = toc_depth, includes = includes) %>%
        purrr::discard(is.null)
    )
  )

  output
}
