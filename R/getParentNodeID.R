#' Get complete tree branch paths for a given terminal node
#'
#' This recursive function to get the intermediate nodes across Root-Terminal paths (i.e. From a Root node to a Terminal node)
#' @param i_nodeID,
#' @param i_nodeInfo
#' @keywords regression tree, branch, path
#' @export
#'
#'
getParentNodeID <- function(i_nodeID, i_nodeInfo) {

  parent_nodeID <- i_nodeInfo$parentNodeID[which(i_nodeInfo$nodeID == i_nodeID[length(i_nodeID)])][1]

  if (is.na(parent_nodeID)) {
    return(i_nodeID)
  } else {
    return(getParentNodeID(c(i_nodeID, parent_nodeID), i_nodeInfo))
  }
}
