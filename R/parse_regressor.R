#' Parse GUIDE regressors
#'
#' This function parses a GUIDE regressor file.
#' @param outfpath
#' @keywords GUIDE, output, regressor, parse
#' @export
#'
parse_regressor <- function(outfpath) {

  # Read in GUIDE output file
  v_OutputData <- readLines(outfpath)

  return(v_OutputData)

}
