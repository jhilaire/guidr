#' Allocate data to Terminal nodes
#'
#' This function allocates data to terminal nodes
#' @param i_data,
#' @param i_nodeInfo
#' @param DEBUG
#' @keywords allocation, data, terminal node, regression tree
#' @export
#'
#'
allocateDataToNodes <- function(i_data, i_nodeInfo, DEBUG=FALSE) {

  if (DEBUG) {
    print("[DEBUG] Showing i_data & i_nodeInfo:")
    print(head(i_data))
    print(head(i_nodeInfo))
  }

  # Get all terminal nodes
  if (DEBUG) print("[DEBUG] allocateDataToNodes: Get all terminal nodes")
  idTnodes <- which(i_nodeInfo$nodeType == "Terminal node")
  tNodeIDs <- i_nodeInfo$nodeID[idTnodes]

  paths <- lapply(tNodeIDs, function(x) getParentNodeID(x,i_nodeInfo))

  # Get list of function sequences
  if (DEBUG) print("[DEBUG] allocateDataToNodes: Get list of function sequences")
  llfs <- lapply(
    # 1. Generate paths
    lapply(
      paths,
      function(path) {
        as.numeric(rev(path))
      }
    ),
    function(path) {
      # 3. Transform rules to functions
      lapply(
        # 2. Generate list of rules from paths
        # TODO: Check if this produces the right output
        lapply(
          1:(length(path)-1),
          function(node) {
            tmp <- i_nodeInfo[which(i_nodeInfo$nodeID == path[node]),][which(as.numeric(strsplit(i_nodeInfo$childNodeIDs[which(i_nodeInfo$nodeID == path[node])][1], ",")[[1]]) == path[node+1]), c(6:11)]
            #tmp <- i_nodeInfo[which(i_nodeInfo$nodeID == path[node]),][which(as.numeric(strsplit(i_nodeInfo$childNodeIDs[which(i_nodeInfo$nodeID == path[node])][1], ",")[[1]]) == path[node+1]), c("variable", "vartype", "operator", "value", "inclNA", "category")]
            #   1           2          3           4        5         6
            #c("variable", "vartype", "operator", "value", "inclNA", "category")
          }),
        function(rule) {
          if (DEBUG) {
            print(paste0("[DEBUG] allocateDataToNodes: ", paste0(rule, collapse=", ")))
          }

          # Numerical variables
          if (paste(rule[2]) %in% c("s", "f", "n")) {
            if (rule[3] == "<=") {
              if (as.logical(rule[5])) {
                out <- function(data) {
                  filter_(data, lazyeval::interp(~ var1 <= var2 | is.na(var1), var1 = as.name(paste(rule[1])), var2 = as.numeric(paste(rule[4])))) %>%
                    mutate(tnode = as.numeric(path[length(path)]))
                }
              } else {
                out <- function(data) {
                  filter_(data, lazyeval::interp(~ var1 <= var2, var1 = as.name(paste(rule[1])), var2 = as.numeric(paste(rule[4])))) %>%
                    mutate(tnode = as.numeric(path[length(path)]))
                }
              }
            }
            if (rule[3] == ">") {
              if (as.logical(rule[5])) {
                out <- function(data) {
                  filter_(data, lazyeval::interp(~ var1 > var2 | is.na(var1), var1 = as.name(paste(rule[1])), var2 = as.numeric(paste(rule[4])))) %>%
                    mutate(tnode = as.numeric(path[length(path)]))
                }
              } else {
                out <- function(data) {
                  filter_(data, lazyeval::interp(~ var1 > var2, var1 = as.name(paste(rule[1])), var2 = as.numeric(paste(rule[4])))) %>%
                    mutate(tnode = as.numeric(path[length(path)]))
                }
              }
            }
            if (rule[3] == "=") {
              out <- function(data) {
                filter_(data, lazyeval::interp(~ is.na(var1), var1 = as.name(paste(rule[1])), var2 = as.numeric(paste(rule[4])))) %>%
                  mutate(tnode = as.numeric(path[length(path)]))
              }
            }
            if (rule[3] == "not") {
              out <- function(data) {
                filter_(data, lazyeval::interp(~ !is.na(var1), var1 = as.name(paste(rule[1])), var2 = as.numeric(paste(rule[4])))) %>%
                  mutate(tnode = as.numeric(path[length(path)]))
              }
            }
          }
          # Categorical variables
          if (paste(rule[2]) %in% c("b", "c")) {
            if (rule[3] == "=") {
              categories <- strsplit(rule[6], ", ")[[1]]
              if (length(categories) == 1) {
                if (as.logical(rule[5])) {
                  out <- function(data) {
                    filter_(data, lazyeval::interp(~ var1 == var2 | is.na(var1), var1 = as.name(paste(rule[1])), var2 = paste(rule[6]))) %>%
                      mutate(tnode = as.numeric(path[length(path)]))
                  }
                } else {
                  out <- function(data) {
                    filter_(data, lazyeval::interp(~ var1 == var2, var1 = as.name(paste(rule[1])), var2 = paste(rule[6]))) %>%
                      mutate(tnode = as.numeric(path[length(path)]))
                  }
                }
              } else {
                if (as.logical(rule[5]) | any(is.na(categories))) {
                  out <- function(data) {
                    filter_(data, lazyeval::interp(~ var1 %in% var2 | is.na(var1), var1 = as.name(paste(rule[1])), var2 = paste0("c(",paste(categories[which(!is.na(categories))], collapse=","),")"))) %>%
                      mutate(tnode = as.numeric(path[length(path)]))
                  }
                } else {
                  out <- function(data) {
                    filter_(data, lazyeval::interp(~ var1 %in% var2, var1 = as.name(paste(rule[1])), var2 = paste0("c(",paste(categories, collapse=","),")"))) %>%
                      mutate(tnode = as.numeric(path[length(path)]))
                  }
                }
              }
            }
            if (rule[3] == "/=") {
              categories <- strsplit(rule[6], ", ")[[1]]
              if (length(categories) == 1) {
                if (as.logical(rule[5])) {
                  out <- function(data) {
                    filter_(data, lazyeval::interp(~ var1 != var2 | is.na(var1), var1 = as.name(paste(rule[1])), var2 = paste(rule[6]))) %>%
                      mutate(tnode = as.numeric(path[length(path)]))
                  }
                } else {
                  out <- function(data) {
                    filter_(data, lazyeval::interp(~ var1 != var2, var1 = as.name(paste(rule[1])), var2 = paste(rule[6]))) %>%
                      mutate(tnode = as.numeric(path[length(path)]))
                  }
                }
              } else {
                if (as.logical(rule[5]) | any(is.na(categories))) {
                  out <- function(data) {
                    filter_(data, lazyeval::interp(~ !var1 %in% var2 | is.na(var1), var1 = as.name(paste(rule[1])), var2 = paste0("c(",paste(categories[which(!is.na(categories))], collapse=","),")"))) %>%
                      mutate(tnode = as.numeric(path[length(path)]))
                  }
                } else {
                  out <- function(data) {
                    filter_(data, lazyeval::interp(~ !var1 %in% var2, var1 = as.name(paste(rule[1])), var2 = paste0("c(",paste(categories, collapse=","),")"))) %>%
                      mutate(tnode = as.numeric(path[length(path)]))
                  }
                }
              }
            }
          }
          return(out)
        }
      )
    }
  )

  myfreduce <- function(i_lfs) {
    if (length(i_lfs) == 1) {
      i_lfs[[1]](i_data)
    } else {
      return(i_lfs[[1]](myfreduce(i_lfs[2:length(i_lfs)])))
    }
  }

  # Perform allocation
  if (DEBUG) print("[DEBUG] allocateDataToNodes: Perform allocation")
  out <- do.call("rbind", lapply(llfs, myfreduce))

  return(out)

}
