#' Generate scatter pots for a particular country
#'
#' This function generates a scatter plot for all variables
#' @param i_alloc,
#' @param i_nodeInfo
#' @param i_iso
#' @param i_var_dep
#' @keywords plot, scatter, regression tree
#' @export
#'
#'
plot_ScatterCountry_allVars <- function(i_alloc, i_nodeInfo, i_iso, i_var_dep, PLOT_LABELS=FALSE) {

  p_shift_x = 0.00
  p_shift_y = 0.03

  # Initialisation
  # Get X and Y limits
  # xlim = c(min(i_alloc[,i_var_x]),  max(i_alloc[,i_var_x]))
  # ylim = c(min(i_alloc[,i_var_y]), max(i_alloc[,i_var_y]))
  # xlim = c(min(i_alloc[which(i_alloc$iso %in% i_iso),i_var_x]), max(i_alloc[which(i_alloc$iso %in% i_iso),i_var_x]))
  # ylim = c(min(i_alloc[which(i_alloc$iso %in% i_iso),i_var_y]), max(i_alloc[which(i_alloc$iso %in% i_iso),i_var_y]))

  tmp = i_alloc %>%
    gather(variable_x, value_x, -iso, -year, -log_E_CC, -tnode)

  # Plot all data points
  p = ggplot(data=tmp %>% filter(iso %in% i_iso) %>% mutate(iso = factor(iso, levels=i_iso))) +
    geom_point(aes_string(x="value_x", y=i_var_dep, fill=i_var_dep, colour=i_var_dep), data=tmp %>% select(-iso), pch=21, alpha=0.33)

  # Compute segment information of intermediate nodes
  # tmp <- i_nodeInfo[which(!duplicated(i_nodeInfo$nodeID)),] %>%  #i_nodeInfo$nodeType == "Intermediate node" &
  #   select(nodeID, variable, value) %>%
  #   filter(variable %in% c(i_var_x, i_var_y)) %>%
  #   mutate(nx    = ifelse(variable == i_var_x, value, xlim[1])) %>%
  #   mutate(nxend = ifelse(variable == i_var_x, value, xlim[2] + 0.05*(xlim[2]-xlim[1]) + p_shift_x*2*(xlim[2]-xlim[1]))) %>%
  #   mutate(ny    = ifelse(variable == i_var_y, value, ylim[1] - 0.05*(ylim[2]-ylim[1]) - p_shift_y*2*(ylim[2]-ylim[1]))) %>%
  #   mutate(nyend = ifelse(variable == i_var_y, value, ylim[2])) %>%
  #   group_by(variable) %>%
  #   arrange(value) %>%
  #   mutate(nxlab = ifelse(variable == i_var_x, value, xlim[2] + 0.05*(xlim[2]-xlim[1]) + p_shift_x*row_number()%%3*(xlim[2]-xlim[1]))) %>%
  #   mutate(nylab = ifelse(variable == i_var_y, value, ylim[1] - 0.05*(ylim[2]-ylim[1]) - p_shift_y*row_number()%%3*(ylim[2]-ylim[1]))) %>%
  #   ungroup()

  # Plot segments
  # p = p +
  #   geom_segment(aes(x=nx, xend=nxend, y=ny, yend=nyend, group=nodeID), data=tmp, linetype=2) +
  #   geom_label(aes(x=nxlab, y=nylab, label=nodeID), colour="black", data=tmp)

  # Plot country data (transition)
  p = p +
    geom_path(aes_string(x="value_x", y=i_var_dep),                  data=tmp %>% filter(iso %in% i_iso) %>% arrange(year), colour="black", size=1.25) +
    geom_point(aes_string(x="value_x", y=i_var_dep, fill=i_var_dep), data=tmp %>% filter(iso %in% i_iso)                  , colour="black", size=4, pch=21) +
    facet_grid(variable_x~iso, scales = "free")
  if (PLOT_LABELS) {
    p = p +
      geom_label_repel(aes_string(x="value_x", y=i_var_dep, fill=i_var_dep, label="year.tnode"),
                       data=tmp %>% filter(iso %in% i_iso) %>% mutate(year.tnode = paste0(year, " (", tnode, ")")),
                       colour="white", fontface = "bold")
  }

  # # Plot options and cosmetics
  # xlim[2] = xlim[2] + 0.05*(xlim[2]-xlim[1]) + p_shift_x*2*(xlim[2]-xlim[1])
  # ylim[1] = ylim[1] - 0.05*(ylim[2]-ylim[1]) - p_shift_y*2*(ylim[2]-ylim[1])

  p = p +
    theme_bw() +
    scale_colour_gradient(low = "#F5F5DC", high = "#8B0000", space = "Lab", na.value = "grey50", guide = "colourbar") +
    scale_fill_gradient(low = "#F5F5DC", high = "#8B0000", space = "Lab", na.value = "grey50", guide = "colourbar") +
    theme(legend.position="none")

  print(p)

  return(p)
}
