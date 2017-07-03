#' Print message with return carriage
#'
#' This function is a modification of the original cat function that appends a return carriage character at the end of a submitted string
#' @param i_text
#' @keywords cat, message, return carriage
#' @export
#'
catret <- function(i_text, ...) {
  cat(paste0(i_text,"\n"), ...)
}
