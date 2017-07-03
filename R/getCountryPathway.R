#' Get country pathway of ISO
#'
#' This function generates a scatter plot
#' @param i_alloc,
#' @param i_iso
#' @keywords country, pathway, regression tree
#' @export
#'
#'
getCountryPathway <- function(i_alloc, i_iso) {

  # Get vector of terminal nodes and associated years for country i_iso
  v        <- i_alloc$tnode[which(i_alloc$iso == i_iso)]
  names(v) <- i_alloc$year[which(i_alloc$iso == i_iso)]
  v        <- v[order(names(v))]

  # v2 = i_alloc$tnode[which(i_alloc$iso == i_iso)][which(duplicated(i_alloc$tnode[which(i_alloc$iso == i_iso)]) == FALSE)]
  # names(v2) = names(v)[which(duplicated(i_alloc$tnode[which(i_alloc$iso == i_iso)]) == FALSE)]
  # names(v2) <- paste0(as.numeric(names(v2)), "-", c((as.numeric(names(v2))-lag(as.numeric(names(v2))))[2:length(as.numeric(names(v2)))]-1, 0) + as.numeric(names(v2)))
  # names(v2)[length(v2)] <- paste0(strsplit(names(v2)[length(v2)], "-")[[1]][1], "-", max(as.numeric(names(v))))

  pathway <- list()  # Initialise list of pathways
  kcnt    <- 1       # Initialise pathway counter

  # Loop over unqiue nodes
  for (knode in unique(v)) {

    # Identify position of current node
    ids = which(v == knode)

    # Find lags and cuts
    lags = as.numeric(names(v[which(v == knode)])) - lag(as.numeric(names(v[which(v == knode)])), default = min(as.numeric(names(v[which(v == knode)])))-1)
    cuts = which(lags != 1)

    # Get initial and final position of
    startid = min(ids)
    endid   = max(ids)

    # If the countries goes in and out the current node several times ...
    if (length(cuts) != 0) {
      # Loop over the different time periods
      for (kcut in 1:length(cuts)) {
        # Update endid
        endid <- ids[cuts[kcut]-1]

        # Add pathway
        pathway[[kcnt]] <-
          data.frame(start=as.numeric(names(v)[startid]), end=as.numeric(names(v)[endid]), period=paste0(names(v)[startid],"-",names(v)[endid]), node=knode)

        # Update startid
        startid <- ids[cuts[kcut]]

        # Update pathway counter
        kcnt <- kcnt +1
      }
      # Update endif
      endid <- max(ids)

      # Add pathway
      pathway[[kcnt]] <-
        data.frame(start=as.numeric(names(v)[startid]), end=as.numeric(names(v)[endid]), period=paste0(names(v)[startid],"-",names(v)[endid]), node=knode)
    #... if the country goes in/out the current node only once
    } else {
      pathway[[kcnt]] <-
        data.frame(start=as.numeric(names(v)[startid]), end=as.numeric(names(v)[endid]), period=paste0(names(v)[startid],"-",names(v)[endid]), node=knode)
    }
    # Update pathway counter
    kcnt <- kcnt +1

  }

  pathway <- do.call("rbind", pathway) %>%
    mutate(type=ifelse(start == end, "S", "M"))

  pathway <- pathway %>%
    left_join(
      pathway %>%
        filter(!duplicated(node)) %>%
        mutate(pos=1:length(unique(node))) %>%
        select(node, pos),
      by=c("node")
    ) %>%
    arrange(start)

  return(pathway)
}
