#' Get warnings from GUIDE output
#'
#' This function extract warning messages in a GUIDE output file.
#' @param outfpath
#' @keywords GUIDE, output, parse, warning
#' @export
#'

get_warnings <- function(outfpath) {

  # Read in GUIDE output file
  v_OutputData <- readLines(outfpath)

  cat("\n------ GUIDE Warnings ------\n")
  cat(paste0(trimws(v_OutputData[which(grepl("Warning", v_OutputData))]), collapse="\n"))
  cat("\n----------------------------\n\n")

}
