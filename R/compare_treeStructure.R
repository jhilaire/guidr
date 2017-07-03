#' Compare tree structures
#'
#' This function computes a similarity index based on the edit distance of 2 tree structures
#' @param i_tree1,
#' @param i_tree2
#' @keywords regression tree, similarity index
#' @export
#'
#'
compare_treeStructure <- function(i_tree1, i_tree2, ...) {

  tmp        <- i_tree1 %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% arrange(desc(n)) %>% mutate(levelsid1 = paste0(nodeID, " (", n, ")"))
  tNodeIDs1  <- tmp$nodeID
  levelsID1  <- tmp$levelsid1
  treepaths1 <- lapply(tNodeIDs1, function(x) {
    nodes <- getParentNodeID(x, i_tree1)[-1]
    out <- i_tree1 %>%
      filter(nodeID %in% nodes) %>%
      group_by(nodeID) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      select(variable, value) %>%
      unite(col = "varval", variable, value, sep = "#")
    return(out$varval)
  })
  comb1 <- unique(unlist(treepaths1))

  tmp        <- i_tree2 %>% filter(nodeType == "Terminal node") %>% select(nodeID,n) %>% arrange(desc(n)) %>% mutate(levelsid2 = paste0(nodeID, " (", n, ")"))
  tNodeIDs2  <- tmp$nodeID
  levelsID2  <- tmp$levelsid2
  treepaths2 <- lapply(tNodeIDs2, function(x) {
    nodes <- getParentNodeID(x, i_tree2)[-1]
    out <- i_tree2 %>%
      filter(nodeID %in% nodes) %>%
      group_by(nodeID) %>%
      filter(row_number() == 1) %>%
      ungroup() %>%
      select(variable, value) %>%
      unite(col = "varval", variable, value, sep = "#")
    return(out$varval)
  })
  comb2 <- unique(unlist(treepaths2))

  out <- compute_editDistance(comb1, comb2, ...)

  return(out)
}
