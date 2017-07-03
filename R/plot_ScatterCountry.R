#' Generate a scatter plot for a particular country
#'
#' This function generates a scatter plot
#' @param i_alloc,
#' @param i_nodeInfo
#' @param i_iso
#' @param i_var_x
#' @param i_var_y
#' @param i_var_col
#' @keywords plot, scatter, regression tree
#' @export
#'
#'
plot_ScatterCountry <- function(i_alloc, i_nodeInfo, i_iso, i_var_x, i_var_y, i_var_col, PLOT_LABELS=FALSE) {

  p_shift_x = 0.00
  p_shift_y = 0.03

  # Initialisation
  # Get X and Y limits
  xlim = c(min(i_alloc[,i_var_x]),  max(i_alloc[,i_var_x]))
  ylim = c(min(i_alloc[,i_var_y]), max(i_alloc[,i_var_y]))
  xlim = c(min(i_alloc[which(i_alloc$iso %in% i_iso),i_var_x]), max(i_alloc[which(i_alloc$iso %in% i_iso),i_var_x]))
  ylim = c(min(i_alloc[which(i_alloc$iso %in% i_iso),i_var_y]), max(i_alloc[which(i_alloc$iso %in% i_iso),i_var_y]))

  # Plot all data points
  p = ggplot(data=i_alloc %>% filter(iso %in% i_iso) %>% mutate(iso = factor(iso, levels=i_iso))) +
    geom_point(aes_string(x=i_var_x, y=i_var_y, fill=i_var_col, colour=i_var_col), data=i_alloc %>% select(-iso), pch=21, alpha=0.33)

  # Compute segment information of intermediate nodes
  tmp <- i_nodeInfo[which(!duplicated(i_nodeInfo$nodeID)),] %>%  #i_nodeInfo$nodeType == "Intermediate node" &
    select(nodeID, variable, value) %>%
    filter(variable %in% c(i_var_x, i_var_y)) %>%
    mutate(nx    = ifelse(variable == i_var_x, value, xlim[1])) %>%
    mutate(nxend = ifelse(variable == i_var_x, value, xlim[2] + 0.05*(xlim[2]-xlim[1]) + p_shift_x*2*(xlim[2]-xlim[1]))) %>%
    mutate(ny    = ifelse(variable == i_var_y, value, ylim[1] - 0.05*(ylim[2]-ylim[1]) - p_shift_y*2*(ylim[2]-ylim[1]))) %>%
    mutate(nyend = ifelse(variable == i_var_y, value, ylim[2])) %>%
    group_by(variable) %>%
    arrange(value) %>%
    mutate(nxlab = ifelse(variable == i_var_x, value, xlim[2] + 0.05*(xlim[2]-xlim[1]) + p_shift_x*row_number()%%3*(xlim[2]-xlim[1]))) %>%
    mutate(nylab = ifelse(variable == i_var_y, value, ylim[1] - 0.05*(ylim[2]-ylim[1]) - p_shift_y*row_number()%%3*(ylim[2]-ylim[1]))) %>%
    ungroup()

  # Plot segments
  p = p +
    geom_segment(aes(x=nx, xend=nxend, y=ny, yend=nyend, group=nodeID), data=tmp, linetype=2) +
    geom_label(aes(x=nxlab, y=nylab, label=nodeID), colour="black", data=tmp)

  # Plot country data (transition)
  # for (kc in i_iso) {
  #   p = p +
  #     geom_path(aes_string(x=i_var_x, y=i_var_y),                  data=i_alloc %>% filter(iso == kc) %>% arrange(year), colour="black", size=1.25) +
  #     geom_point(aes_string(x=i_var_x, y=i_var_y, fill=i_var_col), data=i_alloc %>% filter(iso == kc)                  , colour="black", size=4, pch=21)
  #   if (PLOT_LABELS) {
  #     p = p +
  #       geom_label_repel(aes_string(x=i_var_x, y=i_var_y, fill=i_var_col, label="year.tnode"),
  #                        data=i_alloc %>% filter(iso == kc) %>% mutate(year.tnode = paste0(year, " (", tnode, ")")),
  #                        colour="white", fontface = "bold")
  #   }
  # }
  # p = p +
  #   geom_path(aes_string(x=i_var_x, y=i_var_y, colour="iso"),                  data=i_alloc %>% arrange(year), size=1.25) +
  #   geom_point(aes_string(x=i_var_x, y=i_var_y, fill=i_var_col, colour="iso"), data=i_alloc                  , size=4, pch=21)
  # if (PLOT_LABELS) {
  #   p = p +
  #     geom_label_repel(aes_string(x=i_var_x, y=i_var_y, fill=i_var_col, label="year.tnode"),
  #                      data=i_alloc %>% mutate(year.tnode = paste0(year, " (", tnode, ")")),
  #                      colour="white", fontface = "bold")
  # }
  p = p +
    geom_path(aes_string(x=i_var_x, y=i_var_y),                  data=i_alloc %>% filter(iso %in% i_iso) %>% arrange(year), colour="black", size=1.25) +
    geom_point(aes_string(x=i_var_x, y=i_var_y, fill=i_var_col), data=i_alloc %>% filter(iso %in% i_iso)                  , colour="black", size=4, pch=21) +
    facet_wrap(~iso, ncol=ifelse(length(i_iso) %/% 6 == 0, length(i_iso), 6))
  if (PLOT_LABELS) {
    p = p +
      geom_label_repel(aes_string(x=i_var_x, y=i_var_y, fill=i_var_col, label="year.tnode"),
                       data=i_alloc %>% filter(iso %in% i_iso) %>% mutate(year.tnode = paste0(year, " (", tnode, ")")),
                       colour="white", fontface = "bold")
  }

  # Plot options and cosmetics
  xlim[2] = xlim[2] + 0.05*(xlim[2]-xlim[1]) + p_shift_x*2*(xlim[2]-xlim[1])
  ylim[1] = ylim[1] - 0.05*(ylim[2]-ylim[1]) - p_shift_y*2*(ylim[2]-ylim[1])

  p = p +
    theme_bw() +
    scale_colour_gradient(low = "#F5F5DC", high = "#8B0000", space = "Lab", na.value = "grey50", guide = "colourbar") +
    scale_fill_gradient(low = "#F5F5DC", high = "#8B0000", space = "Lab", na.value = "grey50", guide = "colourbar") +
    theme(legend.position="none") +
    xlim(xlim) + ylim(ylim)

  print(p)

  return(p)
}
