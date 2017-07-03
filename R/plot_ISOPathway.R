#' Plot pathwas of a single ISO
#'
#' This function generates a scatter plot
#' @param i_pathway,
#' @param i_nodeInfo
#' @keywords plot, pathway, regression tree
#' @export
#'
#'
plot_ISOPathway <- function(i_iso, i_alloc, i_nodeInfo) {

  i_pathway <- getCountryPathway(i_alloc, i_iso)

  p = ggplot() +
    geom_point(aes(x=pos, y=0), size=10, color="black",
               data=i_pathway %>% filter(!duplicated(node))) +
    geom_text(aes(x=pos, y=0, label=node), color="white",
              data=i_pathway %>% filter(!duplicated(node)))

  multinodes = i_pathway$node[duplicated(i_pathway$node)]

  p = p +
    geom_text(aes(x=pos, y=-0.19, label=period, angle=90),
              data=i_pathway %>% filter(!node %in% multinodes), color="black", hjust=1) +
    geom_segment(aes(x=pos, xend=pos, y=-0.03, yend=-0.18), data=i_pathway %>% filter(!node %in% multinodes), colour="lightgrey")

  for (knode in multinodes) {
    pos_cur  = unique(i_pathway$pos[which(i_pathway$node == knode)])
    nperiods = length(which(i_pathway$node == knode))
    margin   = 0.01
    space    = (1 - 2*margin)/(nperiods+1)

    for (k in 1:nperiods) {
      tmp  = i_pathway[which(i_pathway$node == knode)[k], ]
      tmp$pos = pos_cur-0.5+margin+space + (k-1)*space
      tmp$pos_cur = pos_cur
      tmp$y    = -0.18
      tmp$yend = -0.03

      p = p +
        geom_text(aes(x=pos, y=-0.19, label=period, angle=90),
                  data=tmp, color="black", hjust=1) +
        geom_segment(aes(x=pos, xend=pos_cur, y=y, yend=yend), data=tmp, colour="lightgrey")
    }
  }

  tmp = i_pathway %>% arrange(start)
  tmp$curvature = -0.5

  # Find duplicated transitions
  transitions = paste0(lag(tmp$node)[-1], "-", tmp$node[-1])
  for (k in unique(transitions)) {
    nduplicates = length(which(transitions == k)) -1
    if (nduplicates != 0) {
      for (kd in 1:nduplicates) {
        cur_id = which(transitions == k)[kd+1]
        tmp$curvature[cur_id] = -0.5 + -0.1*kd
      }
    }

  }

  for (k in 1:(dim(tmp)[1]-1)) {

    node_cur  = tmp$node[k]
    node_next = tmp$node[k+1]

    pos_cur   = tmp$pos[k]
    pos_next  = tmp$pos[k+1]

    curv      = tmp$curvature[k]

    if (pos_cur < pos_next) {
      p = p +
        geom_curve(aes(x=pos_cur, xend=pos_next, y=y, yend=y),
                   data = data.frame(pos_cur=pos_cur, pos_next=pos_next, y=0.03),
                   curvature=curv,
                   arrow = grid::arrow(length = grid::unit(0.015, "npc")))
    } else {
      p = p +
        geom_curve(aes(x=pos_cur, xend=pos_next, y=y, yend=y),
                   data = data.frame(pos_cur=pos_cur, pos_next=pos_next, y=-0.03),
                   curvature=curv,
                   arrow = grid::arrow(length = grid::unit(0.015, "npc")))
    }
  }

  for (knode in unique(i_pathway$node)) {
    pos_cur = i_pathway$pos[which(i_pathway$node == knode)][1]

    tmp <- getPathNodeInfo(knode, i_nodeInfo) %>%
      mutate(nodeID=as.numeric(paste(nodeID))) %>%
      arrange(desc(nodeID))
    tmp <- tmp %>%
      mutate(xpos = pos_cur) %>%
      mutate(ypos = 0.1 + 0.1*1:dim(tmp)[1])

    p = p +
      geom_point(aes(x=xpos, y=ypos, colour=variable), size=8,
                 data=tmp) +
      geom_text(aes(x=xpos, y=ypos, label=nodeID), color="white",
                data=tmp, size=3) +
      geom_text(aes(x=xpos, y=ypos-0.03, label=variable), color="black",
                data=tmp, size=3) +
      geom_text(aes(x=xpos, y=ypos-0.05, label=paste(operator,value)), color="black",
                data=tmp, size=3)
  }

  p = p +
    theme_bw() +
    ylim(-0.3,1) +
    ylab("") + ylab("") +
    theme(axis.line=element_blank(),
          axis.text.x=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank(),
          legend.position="none",
          panel.background=element_blank(),
          panel.border=element_blank(),
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          plot.background=element_blank())
  print(p)

  return(p)
}
