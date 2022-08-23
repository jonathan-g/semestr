#' @import rlang

#' @importFrom stats na.omit
#' @importFrom utils URLencode head tail
NULL

globalVariables(c(".", "where"))

# Dummy function to shut up spurious R CMD check warnings
#' @noRd
dummy <- function() {
  dbplyr::sql
}
