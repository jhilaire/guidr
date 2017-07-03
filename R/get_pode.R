#' Get the proportion of deviance explained
#'
#' This function get the proportion of deviance explained from a GUIDE output file.
#' @param outfpath
#' @keywords GUIDE, output, parse, pode
#' @export
#'
#'
get_pode <- function(outfpath) {
  # Read in GUIDE output file
  v_OutputData <- readLines(outfpath)

  out <- as.numeric(trimws(strsplit(grep("Proportion of deviance explained by tree model", v_OutputData, value = TRUE), ":", fixed=TRUE)[[1]][2]))

  return(out)
}
