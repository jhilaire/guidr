#' Parse GUIDE output
#'
#' This function get tree structure information from a GUIDE output file
#' @param outfpath
#' @param vardef
#' @keywords GUIDE, output, parse
#' @export
#'
parse_output <- function(outfpath, vardef) {

  # Read in GUIDE output file
  v_OutputData <- readLines(outfpath)

  longTree <- FALSE
  if (v_OutputData[19] == " Longitudinal data with T variables") longTree <- TRUE

  # Get node information
  if (v_OutputData[26] == " Piecewise constant model") {
    v_nodeInfoLineStart <- grep("        Node    Total    Cases Matrix    Node      Node      Split          Interacting", v_OutputData) +2

    v_nodeInfoLineEnd   <- grep("Number of terminal nodes of final tree: ", v_OutputData, fixed = TRUE) -2
    if (any(grepl("Warning: tree very large, omitting node numbers in LaTeX file", v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]))) v_nodeInfoLineEnd <- grep("Warning: tree very large, omitting node numbers in LaTeX file", v_OutputData, fixed = TRUE) -1

    widths <- c(
      as.numeric(regexpr("label",    v_OutputData[v_nodeInfoLineStart-1])+5)-1,
      as.numeric(regexpr("cases",    v_OutputData[v_nodeInfoLineStart-1])+5)-1,
      as.numeric(regexpr("fit",      v_OutputData[v_nodeInfoLineStart-1])+4)-1,
      as.numeric(regexpr("rank",     v_OutputData[v_nodeInfoLineStart-1])+4)-1,
      as.numeric(regexpr("D-mean",   v_OutputData[v_nodeInfoLineStart-1])+8)-1,
      as.numeric(regexpr("variable", v_OutputData[v_nodeInfoLineStart-1]))-2,
      as.numeric(regexpr("variable", v_OutputData[v_nodeInfoLineStart-1]))-2+100
    )
    nodeInfo <- read.fwf(textConnection(v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]),
                         widths = c(widths[1], diff(widths)),
                         col.names  = c("nodeID", "n", "nfit", "mrank", "dmean", "dev", "var_split_interactive"),
                         colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "character"),
                         stringsAsFactors =  FALSE, fill = TRUE)

    # nodeInfo <- read.table(textConnection(v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]),
    #                        col.names  = c("nodeID", "n", "nfit", "mrank", "dmean", "dev", "var_split", "var_interactive"),
    #                        colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character"),
    #                        stringsAsFactors =  FALSE, fill = TRUE)
    nodeInfo <- nodeInfo %>%
      mutate(nodeID = gsub("T", "", nodeID))
  }
  if (v_OutputData[26] %in% c(" Piecewise simple linear or constant model", " Piecewise linear model")) {
    v_nodeInfoLineStart <- grep("        Node    Total    Cases Matrix    Node      Node    Split          Other", v_OutputData) +2

    v_nodeInfoLineEnd   <- grep("Number of terminal nodes of final tree: ", v_OutputData, fixed = TRUE) -2
    if (any(grepl("Warning: tree very large, omitting node numbers in LaTeX file", v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]))) v_nodeInfoLineEnd <- grep("Warning: tree very large, omitting node numbers in LaTeX file", v_OutputData, fixed = TRUE) -1


    widths <- c(
      as.numeric(regexpr("label",    v_OutputData[v_nodeInfoLineStart-1])+5)-1,
      as.numeric(regexpr("cases",    v_OutputData[v_nodeInfoLineStart-1])+5)-1,
      as.numeric(regexpr("fit",      v_OutputData[v_nodeInfoLineStart-1])+3)-1,
      as.numeric(regexpr("rank",     v_OutputData[v_nodeInfoLineStart-1])+4)-1,
      as.numeric(regexpr("D-median", v_OutputData[v_nodeInfoLineStart-1])+8)-1,
      as.numeric(regexpr("variable", v_OutputData[v_nodeInfoLineStart-1]))-2,
      as.numeric(regexpr("variable", v_OutputData[v_nodeInfoLineStart-1]))-2+100
    )

    # nodeInfo <- read.table(textConnection(v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]),
    #                        col.names  = c("nodeID", "n", "nfit", "mrank", "dmean", "dev", "var_split", "var_other"),
    #                        colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character"),
    #
    #                        stringsAsFactors =  FALSE, fill = TRUE)
    nodeInfo <- read.fwf(textConnection(v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]),
                         widths = c(widths[1], diff(widths)),
                         col.names  = c("nodeID", "n", "nfit", "mrank", "dmean", "dev", "var_split_and_other"),
                         colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "character"),
                         stringsAsFactors =  FALSE, fill = TRUE)
    nodeInfo <- nodeInfo %>%
      mutate(nodeID = trimws(gsub("T", "", nodeID)))
  }
  # if (v_OutputData[26] %in% c(" Piecewise constant model")) {
  #   v_nodeInfoLineStart <- grep("        Node    Total    Cases Matrix    Node      Node      Split          Interacting", v_OutputData) +2
  #
  #   v_nodeInfoLineEnd   <- grep("Number of terminal nodes of final tree: ", v_OutputData, fixed = TRUE) -2
  #   if (any(grepl("Warning: tree very large, omitting node numbers in LaTeX file", v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]))) v_nodeInfoLineEnd <- grep("Warning: tree very large, omitting node numbers in LaTeX file", v_OutputData, fixed = TRUE) -1
  #
  #
  #   widths <- c(
  #     as.numeric(regexpr("label",    v_OutputData[v_nodeInfoLineStart-1])+5)-1,
  #     as.numeric(regexpr("cases",    v_OutputData[v_nodeInfoLineStart-1])+5)-1,
  #     as.numeric(regexpr("fit",      v_OutputData[v_nodeInfoLineStart-1])+3)-1,
  #     as.numeric(regexpr("rank",     v_OutputData[v_nodeInfoLineStart-1])+4)-1,
  #     as.numeric(regexpr("D-mean",   v_OutputData[v_nodeInfoLineStart-1])+8)-1,
  #     as.numeric(regexpr("MSE",      v_OutputData[v_nodeInfoLineStart-1])+8)-1,
  #     as.numeric(regexpr("variable", v_OutputData[v_nodeInfoLineStart-1])+15)-1,
  #     as.numeric(regexpr("variable", v_OutputData[v_nodeInfoLineStart-1]))-2+100
  #   )
  #
  #   nodeInfo <- read.fwf(textConnection(v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]),
  #                        widths = c(widths[1], diff(widths)),
  #                        col.names  = c("nodeID",    "n",       "nfit",    "mrank",   "dmean",   "dev",     "var_split", "var_interact"),
  #                        colClasses = c("character", "numeric", "numeric", "numeric", "numeric", "numeric", "character", "character"),
  #                        stringsAsFactors =  FALSE, fill = TRUE)
  #   nodeInfo <- nodeInfo %>%
  #     mutate(nodeID = trimws(gsub("T", "", nodeID)))
  # }
  if (v_OutputData[19] == " Longitudinal data with T variables") {
    v_nodeInfoLineStart <- grep("        Node    Total    Cases      Node    Split", v_OutputData) +2

    v_nodeInfoLineEnd   <- grep("Number of terminal nodes of final tree: ", v_OutputData, fixed = TRUE) -2
    if (any(grepl("Warning: tree very large, omitting node numbers in LaTeX file", v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]))) v_nodeInfoLineEnd <- grep("Warning: tree very large, omitting node numbers in LaTeX file", v_OutputData, fixed = TRUE) -1

    widths <- c(
      as.numeric(regexpr("label",    v_OutputData[v_nodeInfoLineStart-1])+5),
      as.numeric(regexpr("cases",    v_OutputData[v_nodeInfoLineStart-1])+5)-1,
      as.numeric(regexpr("fit",      v_OutputData[v_nodeInfoLineStart-1])+3)-1,
      as.numeric(regexpr("MSE",      v_OutputData[v_nodeInfoLineStart-1])+5)-1,
      as.numeric(regexpr("variable", v_OutputData[v_nodeInfoLineStart-1]))+100
    )

    nodeInfo <- read.fwf(textConnection(v_OutputData[v_nodeInfoLineStart:v_nodeInfoLineEnd]),
                         widths = c(widths[1], diff(widths)),
                         col.names  = c("nodeID", "n", "nfit", "mse", "var_split_and_other"),
                         colClasses = c("character", "numeric", "numeric", "numeric", "character"),
                         stringsAsFactors =  FALSE, fill = TRUE)
    nodeInfo <- nodeInfo %>%
      mutate(nodeID = trimws(gsub("T", "", nodeID)))
  }

  # Get the basic structure information of the regression tree
  #v_treeLineStart <- grep(" Regression tree:", v_OutputData) +2
  v_treeLineStart <- grep(" Regression tree:| Regression tree for longitudinal data:", v_OutputData) +2
  v_treeLineEnd   <-grep(" ***************************************************************", v_OutputData, fixed = TRUE) -2

  v_nbNodes <- v_treeLineEnd - v_treeLineStart +1

  v_tree            <- list()
  v_parent          <- "root"
  v_curparentNodeID <- 0

  # Loop over nodes
  for (kitr in 1:v_nbNodes) {
    v_curLine <- v_OutputData[v_treeLineStart + kitr-1]

    # Tree level
    v_pos <- (as.numeric(regexpr("Node", v_curLine)) - 3)/2

    # Node ID
    v_tmp1      <- strsplit(v_curLine, ":", fixed=TRUE)
    v_curNodeID <- trimws(gsub("Node ", "", v_tmp1[[1]][1]))

    # Splitting variable, value and operator
    v_tmp2        <- strsplit(trimws(v_tmp1[[1]][2]), " ", fixed=TRUE)
    v_curSplitVar <- v_tmp2[[1]][1]
    v_curSplitOp  <- v_tmp2[[1]][2]
    # Correct for longitudinal trees
    if (longTree && v_tmp2[[1]][1] == "Mean") v_curSplitVar <- "Mean cost"
    if (longTree && v_tmp2[[1]][1] == "Mean") v_curSplitOp  <- "="
    v_curSplitVal <- trimws(gsub("\"", "", paste(v_tmp2[[1]][3:length(v_tmp2[[1]])], collapse=" ")))

    # Save data
    v_tree[[kitr]] <- data.frame(position     = v_pos,
                                 nodeID       = v_curNodeID,
                                 parentNodeID = v_curparentNodeID,
                                 variable     = v_curSplitVar,
                                 operator     = v_curSplitOp,
                                 value        = v_curSplitVal,
                                 stringsAsFactors = FALSE)

    v_curparentNodeID <- v_curNodeID

  }

  v_tree <- do.call("rbind", v_tree) %>%
    mutate(nodeType = "")

  # Get additional information of the regression tree
  v_treeLineStart <- grep(" ***************************************************************", v_OutputData, fixed = TRUE) +2
  v_treeLineEnd   <- grep(" Proportion of deviance explained by tree model =", v_OutputData, fixed = TRUE) -3
  v_treeLineSep   <- grep(" ----------------------------", v_OutputData, fixed = TRUE)

  v_nbLines <- v_treeLineEnd - v_treeLineStart +1

  # Loop over edges
  for(kitr in 1:(length(v_treeLineSep)-1)) {

    v_curLineStart <- c(v_treeLineStart, v_treeLineSep)[kitr] +1
    v_curLineEnd   <- c(v_treeLineStart, v_treeLineSep)[kitr+1] -1

    v_curLine <- v_OutputData[v_curLineStart]

    # Line containing Node ID information (always the first one)
    v_tmp1  <- strsplit(v_curLine, ":", fixed=TRUE)
    # Node ID
    v_curNodeID <- trimws(gsub("Node ", "", v_tmp1[[1]][1]))
    # Node Type
    v_curNodeType <- trimws(v_tmp1[[1]][2])

    v_tree$nodeType[which(v_tree$nodeID == v_curNodeID)] <- v_curNodeType

  }

  # Correct tree architecture
  for (klvl in 2:max(v_tree$position)) {
    v_curNodeIDs <- v_tree$nodeID[which(v_tree$position == klvl & v_tree$nodeType == "Intermediate node")]

    for (kid in unique(v_curNodeIDs)) {
      v_tree$parentNodeID[which(v_tree$position == klvl & v_tree$nodeType == "Intermediate node" & v_tree$nodeID == kid)][2] = v_tree$parentNodeID[which(v_tree$position == klvl & v_tree$nodeType == "Intermediate node" & v_tree$nodeID == kid)][1]
    }
  }

  if (length(which(v_tree$childNodeIDs != "" & v_tree$nodeType == "Terminal node" & is.na(v_tree$value))) != 0) {
    v_tree$nodeType[which(v_tree$childNodeIDs != "" & v_tree$nodeType == "Terminal node" & is.na(v_tree$value))]       <- "Intermediate node"
  }
  if (length(which(v_tree$childNodeIDs != "" & v_tree$nodeType == "Terminal node" & !is.na(v_tree$value))) != 0) {
    v_tree$childNodeIDs[which(v_tree$childNodeIDs != "" & v_tree$nodeType == "Terminal node" & !is.na(v_tree$value))] <- ""
  }


  # Define child nodes
  v_tree <- v_tree %>%
    mutate(childNodeIDs = "")
  nodeIDs <- unique(v_tree$parentNodeID)[2:length(unique(v_tree$parentNodeID))]

  # Define parent nodes
  for (kid in nodeIDs) {
    v_tree$childNodeIDs[which(v_tree$nodeID == kid)] <- paste0(unique(v_tree$nodeID[which(v_tree$parentNodeID == kid)]), collapse=",")
  }
  v_tree$nodeType[which(v_tree$operator == "=" & grepl("-mean", v_tree$variable))] = "Terminal node"
  v_tree$nodeType[which(v_tree$operator == "=" & v_tree$variable == "Mean cost")]  = "Terminal node"
  v_tree$nodeType[which(v_tree$nodeID == 1)]     = "Root node"
  v_tree$parentNodeID[which(v_tree$nodeID == 1)] = NA

  # Store NAs/Remove NAs
  v_tree <- v_tree %>%
    mutate(inclNA   = ifelse(grepl("or NaN", value), TRUE, FALSE)) %>%
    mutate(value    = trimws(gsub("or NaN", "", value))) %>%
    mutate(value    = trimws(gsub("NA", "", value))) %>%
    mutate(category = ifelse(is.na(as.numeric(value)),  value, NaN)) %>%
    mutate(value    = ifelse(!is.na(as.numeric(value)), value, NaN)) %>%
    mutate(value    = as.numeric(value))

  v_tree <- left_join(v_tree,
                       vardef %>%
                         select(variable, type) %>%
                         rename(vartype=type) %>%
                         mutate(vartype = paste(vartype)),
                       by=c("variable")) %>%
    select(position, nodeID, nodeType, parentNodeID, childNodeIDs, variable, vartype, operator, value, inclNA, category)


  for (kna in which(is.na(v_tree$value))) {

    val <- v_tree$value[which(v_tree$nodeID == v_tree$nodeID[kna])]

    v_tree$value[kna] <- ifelse(length(val[which(!is.na(val))]) == 0, NA, val[which(!is.na(val))])
  }

  v_tree <- inner_join(v_tree, nodeInfo, by=c("nodeID"))

  return(v_tree)

}
