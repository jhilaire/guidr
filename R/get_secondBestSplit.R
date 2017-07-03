#' Get the second best split variable
#'
#' This function get the second best split variable from a GUIDE output file.
#' @param outfpath
#' @keywords GUIDE, output, parse, second best split
#' @export
#'
#'
get_secondBestSplit <- function(outfpath, REMOVE_LOG=FALSE) {

  text_secBestSplit <- "Second best split variable (based on curvature test) at root node is"

  # Read in GUIDE output file
  v_OutputData <- readLines(outfpath)

  tmp <- grep(substr(text_secBestSplit,1,17), v_OutputData, value = TRUE)

  out <- trimws(substr(tmp, nchar(text_secBestSplit)+2, nchar(tmp)))

  if(REMOVE_LOG) out <- gsub("log_", "", out)

  return(out)
}
