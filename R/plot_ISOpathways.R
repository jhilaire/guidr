#' Plot pathwas of one or several ISOs
#'
#' This function generates a network plot
#' @param i_iso
#' @param i_alloc
#' @param i_tree
#' @keywords plot, pathway, regression tree
#' @export
#'
#'
#'
plot_ISOPathways <- function(i_iso, i_alloc, i_tree, export=NULL) {

  # Get all terminal nodes
  tNodeIDs <- i_tree$nodeID[which(i_tree$nodeType == "Terminal node")]

  # Get pathways
  tmp <- lapply(tNodeIDs, function(i_node) {
    tmp <- i_alloc %>%
      filter(tnode == i_node) %>%
      group_by(iso) %>%
      arrange(year) %>%
      ungroup()  %>%
      select(iso, year) %>%
      group_by(iso) %>%
      mutate(yd = year-lag(year, default=min(year)-1)) %>% mutate(jump=ifelse(yd != 1, 1, 0)) %>%
      mutate(groups = cumsum(jump)) %>%
      select(-yd, -jump) %>%
      ungroup() %>%
      group_by(iso,groups) %>%
      summarise(year_min=min(year), year_max=max(year)) %>%
      ungroup() %>%
      mutate(tnode=i_node)
    return(tmp)
  })
  tmp <- do.call("rbind",tmp) %>%
    arrange(iso, year_min) %>%
    mutate(period = paste0(year_min,"-",year_max)) %>%
    rename(from=tnode) %>%
    mutate(to=from) %>%
    select(iso,year_min,year_max,period,groups,from,to)

  tmp2 <- list()
  cnt <- 1
  for (kiso in unique(tmp$iso)) {
    tmp3 <- tmp %>% filter(iso == kiso)
    nbobs <- nrow(tmp3)
    if (nbobs > 1) {
      for (ko in 1:(nbobs-1)) {
        tmp2[[cnt]] <- data.frame(
          iso      = kiso,
          year_min = as.numeric(tmp3$year_max[ko]),
          year_max = as.numeric(tmp3$year_min[ko+1]),
          period   = paste0(as.numeric(tmp3$year_max[ko]), "-", as.numeric(tmp3$year_min[ko+1])),
          groups   = 0,
          from     = as.numeric(tmp3$from[ko]),
          to       = as.numeric(tmp3$to[ko+1])
        )
        cnt <- cnt+1
      }
    }
  }
  tmp <- tmp %>%
    rbind(do.call("rbind",tmp2)) %>%
    arrange(iso, year_min) %>%
    select(from,to,iso,year_min,year_max,period,groups)



  # Initialise graph
  g <- graph_from_data_frame(tmp %>% filter(iso %in% i_iso),
                             directed = TRUE,
                             vertices = i_tree[which(i_tree$nodeType == "Terminal node"), ] %>%
                               arrange(value) %>%
                               select_("nodeID", "parentNodeID", "childNodeIDs", "variable", "operator", "value"))

  # Color code nodes
  colGrad1 <- colorRampPalette(c("#fef0d9ff", "#b30000ff"))
  colGrad2 <- colorRampPalette(c("#fef0d9ff", "#0000b3ff"))

  cols <- rep(NA, length(V(g)))

  cols1 <- colGrad1(1000)
  cols2 <- colGrad2(1000)

  if (length(which(V(g)$value >= 0)) != 0) cols[as.numeric(V(g)[which(V(g)$value >= 0)])] <- cols1[round(V(g)$value[which(V(g)$value >= 0)]/max(V(g)$value[which(V(g)$value >= 0)])*1000)]
  if (length(which(V(g)$value < 0)) != 0)  cols[as.numeric(V(g)[which(V(g)$value < 0)])]  <- cols2[round(V(g)$value[which(V(g)$value < 0)]/min(V(g)$value[which(V(g)$value < 0)])*1000)]

  vertex_attr(g, "colour") <- cols

  # Color code edges
  ecols <- brewer.pal(9, "Set1")[1:length(i_iso)]

  edge_attr(g, "colour") <- (data.frame(iso=edge.attributes(g)$iso) %>% mutate(col=as.character(factor(iso, levels=i_iso, labels=ecols))))$col
  #edge_attr(g, "width")  <- ifelse(round(edge.attributes(g)$probability*5, digits=0) == 0, 1, round(edge.attributes(g)$probability*5, digits=0))

  # Define layout
  # igraph.drl.options <- igraph.drl.default
  # igraph.drl.options$edge.cut               <- 0.8
  # igraph.drl.options$init.iterations        <- 0
  # igraph.drl.options$init.temperature       <- 2000
  # igraph.drl.options$init.attraction        <- 10 #10
  # igraph.drl.options$init.damping.mult      <- 1
  # igraph.drl.options$liquid.iterations      <- 200
  # igraph.drl.options$liquid.temperature     <- 2000
  # igraph.drl.options$liquid.attraction      <- 10
  # igraph.drl.options$liquid.damping.mult    <- 1
  # igraph.drl.options$expansion.iterations   <- 200
  # igraph.drl.options$expansion.temperature  <- 2000
  # igraph.drl.options$expansion.attraction   <- 2
  # igraph.drl.options$expansion.damping.mult <- 1
  # igraph.drl.options$cooldown.iterations    <- 200
  # igraph.drl.options$cooldown.temperature   <- 2000
  # igraph.drl.options$cooldown.attraction    <- 1
  # igraph.drl.options$cooldown.damping.mult  <- 0.1
  # igraph.drl.options$crunch.iterations      <- 50
  # igraph.drl.options$crunch.temperature     <- 250
  # igraph.drl.options$crunch.attraction      <- 1 # 1
  # igraph.drl.options$crunch.damping.mult    <- 0.25
  # igraph.drl.options$simmer.iterations      <- 100
  # igraph.drl.options$simmer.temperature     <- 250
  # igraph.drl.options$simmer.attraction      <- 0.5 # 0.5
  # igraph.drl.options$simmer.damping.mult    <- 0
  # layout <- layout.drl(g, use.seed = 1, weights = NULL, fixed = NULL, dim = 2, options = igraph.drl.options)
  #layout <- layout.gem(g)
  #layout <- layout.reingold.tilford(g)
  #layout <- layout.grid(g)
  layout <- layout_in_circle(g, order = order(V(g)$value))
  # layout <- layout.graphopt(g)
  # layout <- layout.random(g)
  # layout <- layout.fruchterman.reingold(g)
  # layout <- layout.kamada.kawai(g) # stable
  # layout <- layout.mds(g) # stable
  # layout <- layout.fruchterman.reingold.grid(g) # stable

  # Plot graph
  #png(filename="output/country_pathways.png", width=1000, height=1000)
  plot(g,
       layout = layout,
       # Vertex options
       vertex.size=12,
       vertex.label.cex=0.9,
       vertex.label.family='helvetica bold',
       vertex.label.font=2, # 1: plain text, 2: bold face, 3: italic, 4: bold and italic, 5: symbol font
       vertex.label.color="black",
       vertex.color=V(g)$colour,
       # Edge options
       #edge.width=E(g)$width,
       edge.curved=TRUE,
       edge.label=E(g)$period,
       edge.color =E(g)$colour,
       edge.arrow.size=0.3,
       # Other options
       #asp = 0.4,
       margin=0.2)
  legend("bottom", legend = i_iso, col=ecols, fill=ecols, cex=0.8, ncol = min(10, length(i_iso)))
  #dev.off()

  if (!is.null(export)) saveAsGEXF(g, filepath=export)
}

