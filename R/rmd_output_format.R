#' Make an RMD output format
#'
#' Make an output format for creating blogdown and PDF output.
#'
#' @param toc Include a table of contents?
#'
#' @return A list containing a named list of output formats.
make_rmd_output_format <- function(toc = FALSE) {
  output <- list(
    list(
      "blogdown::html_page" =
        list(md_extensions = get_md_extensions(),
             toc = toc),

      pdf_document =
        list(md_extensions = get_md_extensions(),
             toc = toc,
             includes =
               list(
                 in_header = "ees3310.sty"
               )
        )
    )
  )
  output
}
