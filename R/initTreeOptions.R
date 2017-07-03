#' Initialise tree options
#'
#' This function initialises tree options.
#' @param prune
#' @param nbcv
#' @param cvtype
#' @param sevalue
#' @param search
#' @param splitfrac
#' @param truncmeth
#' @param missregval
#' @param maxsplits
#' @param minnbnodes
#' @keywords GUIDE, tree, model, options
#' @export
#'
initTreeOptions <- function(
  prune      = "1",  # 1=prune by CV, 2=no pruning
  nbcv       = 10,   # Number of cross-validation steps (i.e. number of sub data spaces)
  cvtype     = "1",  # 1=mean-based CV tree, 2=median-based CV tree
  sevalue    = 0.5,  # Standard Error number for pruning [0..1]
  search     = "2",  # 1=split point from quantiles, 2=use exhaustive search
  quantile1  = 0.25, # Option for Quantile regression (with 1 quantile)
  quantile2  = 0.75, # Option for Quantile regression (with 2 quantiles)
  splitfrac  = 1.00, # 1=accept default splitting fraction (i.e. frac, where #splits = max(9,fract*n), with n = #cases in node), 2=change it
  truncmeth  = "3",  # Data truncation method (0=none, 1=node range, 2=+10% node range, 3=global range, 4=2-sided Winsorization)
  missregval = "2",  # missing regressor values: 1=separate models, 2=impute with means, 3=constant model
  maxsplits  = 10,   # max. no. split levels
  minnbnodes = 3     # min. node sample size
  ){

  out   <- data.frame(
    prune      = prune,
    nbcv       = nbcv,
    cvtype     = cvtype,
    sevalue    = sevalue,
    truncmeth  = truncmeth,
    missregval = missregval,
    splitfrac  = splitfrac,
    search     = search,
    maxsplits  = maxsplits,
    minnbnodes = minnbnodes,
    quantile1  = quantile1,
    quantile2  = quantile2
  )

  return(out)
}
