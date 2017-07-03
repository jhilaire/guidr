#' Compute edit distance
#'
#' This function computes the edit distance of two vectors of items
#' @param v1,
#' @param v2
#' @keywords regression tree, edit distance
#' @export
#'
#'
#'
compute_editDistance <- function(v1, v2, SORT=TRUE, NORMALISE=FALSE) {

  symbols <- c(letters,
               LETTERS,
               paste(0:9),
               " ", ",", ";", ".", ":", "-", "_", "!", "§", "$", "%", "&", "/", "(", ")", "=", "?", "`", "´", "*", "+", "~", "'", "#", "<", ">", "|", "°", "^")

  # Sort vectors is necessary
  if (SORT) {
    v1 <- sort(v1)
    v2 <- sort(v2)
  }

  # Generate mapping between splitting vars and associated values and letters
  map_svarval_char <- data.frame(
    svarval = sort(unique(c(v1, v2))),
    char    = symbols[1:length(unique(c(v1, v2)))],
    stringsAsFactors = FALSE)

  # Replace splitting vars and associated values by letters
  out <- stringdist(
    paste0(sapply(v1, function(x) map_svarval_char$char[which(map_svarval_char$svarval == x)]), collapse=""),
    paste0(sapply(v2, function(x) map_svarval_char$char[which(map_svarval_char$svarval == x)]), collapse="")
  )

  if (NORMALISE) out <- out/max(c(length(v1), length(v2)))

  return(out)
}
