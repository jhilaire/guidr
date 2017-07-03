#' Creates fake rpart tree object
#'
#' This function creates a fake rpart tree object for plotting purposes
#' @param i_gtree
#' @keywords plot, GUIDE, tree, prp
#' @export
#'

as.fake.rpart.tree <- function(i_gtree) {

  # Only frame, splits, methods and attribute class are necessary to use the plotting functions from rpart and rpart.plot

  dvar     <- gsub("log_", "",
                   gsub("_IEA|_PWT|_WB|_SWIID|_CDIAC|_UNFCCC|_WGI", "", i_gtree$variable[which(i_gtree$nodeType == "Terminal node")][1]))
  nodes    <- unique(i_gtree$nodeID)
  vars     <- gsub(dvar, "<leaf>",
                   gsub("log_", "",
                        gsub("_IEA|_PWT|_WB|_SWIID|_CDIAC|_UNFCCC|_WGI", "", i_gtree$variable[which(!duplicated(i_gtree$nodeID))])), fixed = TRUE)
  n        <- i_gtree$n[which(!duplicated(i_gtree$nodeID))]
  wt       <- i_gtree$n[which(!duplicated(i_gtree$nodeID))]
  dev      <- i_gtree$dev[which(!duplicated(i_gtree$nodeID))]
  yval     <- i_gtree$dmean[which(!duplicated(i_gtree$nodeID))]
  nb_nodes <- length(nodes)

  # Generate frame
  frame = data.frame(
    # data
    var        = vars,                # a factor giving the names of the variables used in the split at each node (leaf nodes are denoted by the level "<leaf>")
    n          = n,                   # the number of observations reaching the node
    wt         = wt,                  # the sum of case weights for observations reaching the node
    dev        = dev,                 # the deviance of the node
    yval       = yval,                # the fitted value of the response at the node
    complexity = rep(NA, nb_nodes),   # the complexity parameter at which this split will collapse
    ncompete   = rep(0, nb_nodes),    # the number of competitor splits recorded (set to 0, no competitor)
    nsurrogate = rep(0, nb_nodes),    # the number of surrogate splits recorded  (set to 0, no surrogate)
    #yval2      = rep(NA, nb_nodes),
    # options
    row.names  = nodes   # Node IDs
  )

  # Generate splits matrix
  # ncount: number of observations in split
  # ncat  : split category (< or >=)
  #
  intvar_id   <- which(vars != "<leaf>")
  intvar      <- vars[intvar_id]
  index       <- i_gtree$value[which(!duplicated(i_gtree$nodeID))][intvar_id]
  nb_intNodes <- length(intvar)
  splits = matrix(NA, nrow=nb_intNodes, ncol = 5)
  splits[,1] <- n[intvar_id]
  splits[,2] <- rep(-1, nb_intNodes)
  splits[,3] <- -NA
  splits[,4] <- index
  splits[,5] <- NA
  row.names(splits) <- intvar
  colnames(splits)  <- c("count", "ncat", "improve", "index", "adj")

  out <- list(frame   = frame,
              where   = NA, call = NA, terms = NA, cptable = NA,
              method  = "anova",
              parms   = NA, control = NA, functions = NA, numresp = NA,
              splits  = splits,
              variable.importance = NA, y = NA, ordered = NA)

  attr(out, "xlevels") = NA
  attr(out, "ylevels") = NA
  attr(out, "class")   = "rpart"

  return(out)
}
