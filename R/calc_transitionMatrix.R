#' Compute transition matrix
#'
#' This function computes a transition matrix between terminal nodes
#' @param i_data,
#' @param i_nodeInfo
#' @keywords transition matrix, data, terminal node, regression tree
#' @export
#'
#'
calc_transitionMatrix <- function(i_alloc, i_nodeInfo) {

  # Get all terminal nodes
  tNodeIDs <- i_nodeInfo$nodeID[which(i_nodeInfo$nodeType == "Terminal node")]

  #
  data_from <- lapply(tNodeIDs, function(i_node) {
    tmp <- i_alloc %>%
      filter(tnode == i_node) %>%
      group_by(country) %>%
      arrange(year) %>%
      ungroup()  %>%
      select(country, year) %>%
      group_by(country) %>%
      mutate(yd = year-lag(year, default=min(year)-1)) %>% mutate(jump=ifelse(yd != 1, 1, 0)) %>%
      mutate(groups = cumsum(jump)) %>%
      select(-yd, -jump) %>%
      ungroup() %>%
      group_by(country,groups) %>%
      summarise(year_min=min(year), year_max=max(year)) %>%
      ungroup()

    tmp2 <- i_alloc %>%
      filter(country %in% tmp$country) %>%
      select(country, year, tnode) %>%
      left_join(tmp %>% select(country,groups,year_min), by=c("country")) %>%
      group_by(country,groups) %>%
      arrange(year) %>%
      mutate(ym1 = lead(year)) %>%
      ungroup() %>%
      filter(ym1 == year_min) %>%
      select(country,year,tnode) %>%
      mutate(type="Previous") %>%
      group_by(tnode) %>%
      summarize(n=n()) %>%
      ungroup() %>%
      mutate(prob=n/sum(n)) %>%
      select(-n) %>%
      rename(from=tnode) %>%
      mutate(to=i_node) %>%
      select(from,to,prob)

    return(tmp2)
  })
  data_from <- do.call("rbind", data_from)

  data_to <- lapply(tNodeIDs, function(i_node) {
    tmp <- i_alloc %>%
      filter(tnode == i_node) %>%
      group_by(country) %>%
      arrange(year) %>%
      ungroup()  %>%
      select(country, year) %>%
      group_by(country) %>%
      mutate(yd = year-lag(year, default=min(year)-1)) %>% mutate(jump=ifelse(yd != 1, 1, 0)) %>%
      mutate(groups = cumsum(jump)) %>%
      select(-yd, -jump) %>%
      ungroup() %>%
      group_by(country,groups) %>%
      summarise(year_min=min(year), year_max=max(year)) %>%
      ungroup()

    tmp2 <-
      i_alloc %>%
      filter(country %in% tmp$country) %>%
      select(country, year, tnode) %>%
      left_join(tmp %>% select(country,groups,year_max), by=c("country")) %>%
      group_by(country,groups) %>%
      arrange(year) %>%
      mutate(yp1 = lag(year)) %>%
      ungroup() %>%
      filter(yp1 == year_max) %>%
      select(country,year,tnode) %>%
      mutate(type="Next") %>%
      group_by(tnode) %>%
      summarize(n=n()) %>%
      ungroup() %>%
      mutate(prob=n/sum(n)) %>%
      select(-n) %>%
      mutate(from=i_node) %>%
      rename(to=tnode) %>%
      select(from,to,prob)

    return(tmp2)
  })
  data_to <- do.call("rbind", data_to)

  data = rbind(
    data_from,
    data_to
  ) %>%
    rename(probability=prob)

  return(data)
}
