#' @import rlang

#' @importFrom stats na.omit
#' @importFrom utils URLencode head tail
NULL

globalVariables(c(".", "where"))

options(semestr.verbose = getOption("semestr.verbose", default = 1))

# Dummy function to shut up spurious R CMD check warnings
#' @noRd
dummy <- function() {
  dbplyr::sql
}
