#' Get full path of from root node to terminal node
#'
#' This function generates a composite plot for a given terminal node
#' @param i_tnode,
#' @param i_nodeInfo
#' @keywords plot, terminal node, regression tree
#' @export
#'
#'
getPathNodeInfo <- function(i_tnode, i_nodeInfo, DEBUG=FALSE) {

  path <- as.numeric(rev(getParentNodeID(i_tnode, i_nodeInfo)))
  if (DEBUG) print("[DEBUG] getPathNodeInfo: Call to getParentNodeID OK")

  id_nodeID   = which(names(v_tree[[k_tm]][["0"]]) == "nodeID")
  id_varname  = which(names(v_tree[[k_tm]][["0"]]) == "variable")
  id_vartype  = which(names(v_tree[[k_tm]][["0"]]) == "vartype")
  id_operator = which(names(v_tree[[k_tm]][["0"]]) == "operator")
  id_value    = which(names(v_tree[[k_tm]][["0"]]) == "value")
  id_dev      = which(names(v_tree[[k_tm]][["0"]]) == "dev")
  id_inclNA   = which(names(v_tree[[k_tm]][["0"]]) == "inclNA")

  ids <- c(id_nodeID, id_varname, id_vartype, id_operator, id_value, id_dev, id_inclNA)

  # 3. Transform rules to functions
  tmp <- lapply(
    # 2. Generate list of rules from paths
    # TODO: Check if this produces the right output
    lapply(
      1:(length(path)-1),
      function(node) {
        tmp <- i_nodeInfo[which(i_nodeInfo$nodeID == path[node]),][which(as.numeric(strsplit(i_nodeInfo$childNodeIDs[which(i_nodeInfo$nodeID == path[node])][1], ",")[[1]]) == path[node+1]), ids]
      }),
    function(rule) {
      # Variable type (numerical)
      if (paste(rule[3]) %in% c("s", "f", "n")) {
        if (rule[4] == "<=") {
          if (as.logical(rule[7])) {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=paste0(rule[4],"*"), value=rule[5], dev=rule[6])
          } else {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=rule[4],             value=rule[5], dev=rule[6])
          }

        }
        if (rule[4] == ">") {
          if (as.logical(rule[7])) {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=paste0(rule[4],"*"), value=rule[5], dev=rule[6])
          } else {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=rule[4],             value=rule[5], dev=rule[6])
          }

        }
        if (rule[4] == "=") {
          if (as.logical(rule[7])) {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=paste0(rule[4],"*"), value=rule[5], dev=rule[6])
          } else {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=rule[4],             value=rule[5], dev=rule[6])
          }

        }
        if (rule[4] == "not") {
          if (as.logical(rule[7])) {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=paste0(rule[4],"*"), value=rule[5], dev=rule[6])
          } else {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=rule[4],             value=rule[5], dev=rule[6])
          }

        }
      }
      # Categorical variables
      if (paste(rule[3]) %in% c("b", "c")) {
        if (rule[4] == "=") {
          if (as.logical(rule[7])) {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=paste0(rule[4],"*"), value=rule[5], dev=rule[6])
          } else {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=rule[4],             value=rule[5], dev=rule[6])
          }

        }
        if (rule[4] == "/=") {
          if (as.logical(rule[7])) {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=paste0(rule[4],"*"), value=rule[5], dev=rule[6])
          } else {
            out <- data.frame(nodeID=rule[1], variable=rule[2], operator=rule[4],             value=rule[5], dev=rule[6])
          }

      }
      }
      return(out)
    }
  )

  out <- do.call("rbind", tmp)

  return(out)

}
