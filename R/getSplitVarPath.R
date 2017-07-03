#' Get tree branch paths of specific depth (from root node)
#'
#' This function
#' @param i_nodeInfo
#' @param i_depth
#' @keywords regression tree, branch, path
#' @export
#'
#'
getSplitVarPath <- function(i_nodeInfo, i_depth, DIGITS=NULL, REMOVE_LOG=FALSE, REMOVE_MEAN=FALSE) {
  tmp <- lapply(
          lapply(i_nodeInfo$nodeID[which(i_nodeInfo$nodeType == "Terminal node")],
                 function(x) getParentNodeID(x, i_nodeInfo)),
          function(x) {
            depth <- ifelse(length(x) < i_depth, length(x), i_depth)
            if (!is.null(DIGITS)) {
              value <- round(i_nodeInfo$value[which(i_nodeInfo$nodeID %in% rev(x)[1:depth] & !duplicated(i_nodeInfo$nodeID))], digits=DIGITS)
            } else {
              value <- i_nodeInfo$value[which(i_nodeInfo$nodeID %in% rev(x)[1:depth] & !duplicated(i_nodeInfo$nodeID))]
            }
            paste0(
              paste0(i_nodeInfo$variable[which(i_nodeInfo$nodeID %in% rev(x)[1:depth] & !duplicated(i_nodeInfo$nodeID))],
                     "=",
                     value),
              collapse=".")
            })

  selection <- which(unlist(lapply(tmp, function(x) length(strsplit(x, "=")[[1]])-1)) == i_depth)

  tmp <- unlist(tmp[selection])

  tmp <- tmp[which(!duplicated(tmp))]

  if (REMOVE_LOG)  tmp <- gsub("log_", "", tmp)
  if (REMOVE_MEAN) tmp <- gsub("-mean", "", tmp)

  return(tmp)
}
