#' Parse GUIDE regression coefficient file
#'
#' This function read the GUIDE output file containing regression coefficients
#' @param outfpath
#' @keywords GUIDE, output, regression coefficients, parse
#' @export
#'
parse_regcoef <- function(outfpath) {

  # Read in GUIDE output file
  v_OutputData <- read.table(outfpath, header = TRUE)

  return(v_OutputData)

}
