#' Plot a regression tree
#'
#' This function plots a regression tree using the rpart.plot package
#' @param i_tree,
#' @param title
#' @param CEX
#' @param TWEAK
#' @param YESNO
#' @param LEGEND.X
#' @param LEGEND.Y
#' @param LEGEND.NCOL
#' @param LEGEND.CEX
#' @param FILENAME
#' @keywords plot, regression tree
#' @export
#'
#'
plot_tree <- function(i_tree, TITLE="Regression Tree", VARLEN=0, FACLEN=0, UNIFORM=TRUE, COMPRESS=TRUE,
                      COL="black", SPACE=1, YSPACE=SPACE, GAP=NULL, FALLEN.LEAVES=TRUE,
                      SHADOW.OFFSET=0.4,
                      BRANCH=if(FALLEN.LEAVES) 1 else .2, BRANCH.TYPE=0, BRANCH.COL=if (identical(BRANCH.TYPE, 0)) 1 else "gray",
                      CEX=NULL, TWEAK=1.0, YESNO=FALSE, LEGEND.X=NULL, LEGEND.Y=NULL, LEGEND.NCOL=1, LEGEND.CEX=1.0,
                      TRACE=FALSE,
                      FILENAME=NULL) {

  require(rpart.plot)
  require(RColorBrewer)


  if(attr(i_tree, "class") == "rpart") {
    frp <- i_tree
  } else {
    frp <- as.fake.rpart.tree(i_tree)
  }

  if (length(levels(frp$frame$var))-1 > 12) {
    varcols <- c("#000000", rainbow(length(levels(frp$frame$var))-1))
  } else {
    varcols <- c("#000000", brewer.pal(length(levels(frp$frame$var))-1, "Set3")[1:(length(levels(frp$frame$var))-1)])
  }


  names(varcols) <- c("<leaf>", levels(frp$frame$var)[which(levels(frp$frame$var) != "<leaf>")])
  cols     <- data.frame(
    var     = paste(frp$frame$var)) %>%
    mutate(textcol = ifelse(var == "<leaf>", "#FFFFFF", "#000000"))
  cols$boxcol  = paste(sapply(paste(frp$frame$var), function(x) varcols[which(names(varcols) == x)]))


  if (!is.null(FILENAME)) {
    ext <- strsplit(FILENAME, ".", fixed=TRUE)[[1]]
    ext <- ext[length(ext)]
    if (ext == "pdf") pdf(file=FILENAME, paper = "a4r", height = 15, width = 15*4/3)
    if (ext == "svg") svg(file=FILENAME)
    if (ext == "png") png(file=FILENAME, units = "px", height = 1024, width = 2048)
  }

  prp(frp,
      type            = 2,     # Label all nodes, not just leaves and draw the split labels below the node labels.
      extra           = 101,   # Display the number of observations that fall in the node and the percentage of observations in the node
      under           = FALSE, # Applies only if extra > 0. Default FALSE, meaning put the extra text in the box. Use TRUE to put the text under the box
      main            = TITLE,
      clip.right.labs = TRUE,
      nn              = TRUE,  # Displays node number
      ni              = FALSE, # Display the node indices (i.e. the row numbers of the nodes in the object's frame)
      yesno           = YESNO, # displays Yes/No at first split
      fallen.leaves   = FALLEN.LEAVES,  # If TRUE, position the leaf nodes at the bottom of the graph.
      branch          = if (FALLEN.LEAVES) 1 else 0.2,   # Controls the shape of the branch lines. Specify a value between 0 (V shaped branches) and 1 (square shouldered branches).
      uniform         = UNIFORM,# If TRUE (the default), the vertical spacing of the nodes is uniform. If FALSE, the nodes are spaced proportionally to the fit (more precisely, to the difference between a node's deviance and the sum of its two children's deviances).
      left            = TRUE,  # Default TRUE, meaning the left side of a split is the path taken if the split condition is true.
      xflip           = FALSE, #  If TRUE, flip the tree horizontally.
      yflip           = FALSE, # If TRUE, flip the tree vertically, so the root is at the bottom.
      Margin          = 0,     # Extra white space around the tree, as a fraction of the graph width.
      space           = SPACE, #
      gap             = GAP,   # Minimum horizontal gap between the (possibly invisible) boxes, in character widths.
      digits          = 2,     # The number of significant digits in displayed numbers.
      varlen          = VARLEN,# Length of variable names (0 for complete variable names)
      faclen          = FACLEN,# Length of factor names (0 for complete variable names)
      cex             = CEX,   # Default NULL, meaning calculate the text size automatically.
      tweak           = TWEAK, # Adjust the (possibly automatically calculated) cex. Default 1, meaning no adjustment. Use say tweak=1.2 to make the text 20% larger.
      compress        = COMPRESS,  # If TRUE (the default), make more space by shifting nodes horizontally where space is available. This often allows larger text.
      trace           = TRACE, # Default FALSE. Use TRUE to print the automatically calculated cex, xlim, and ylim. Use integer values greater than 1 for more detailed tracing.
      # Snipping options
      snip            = FALSE, # Set TRUE to interactively trim the tree with the mouse.
      snip.fun        = NULL,  # Function invoked after each mouse click when snip=TRUE. Default NULL, meaning no function.
      # Node label options
      box.col         = 0,     # Color of the boxes around the text. Default 0, meaning use the background color.
      box.palette     = "auto",# Palette for coloring the node boxes based on the fitted value.
      pal.thresh      = NULL,  # Applies when box.palette is a two-color diverging palette (such as BuGn). Specifies the response threshold to split the two sub-palettes (such as Bu and Gn).
      pal.node.fun    = FALSE, # Specifies how the box.palette argument is handled when the node.fun argument is specified.
      border.col      = COL,   # Color of the box border around the text. Default col, the color of the text in the box.
      round           = 0,     # Controls the rounding of the corners of the node boxes. Specify 0 for sharp edges, and values greater than 0 for rounded edges.
      leaf.round      = 0.6,   # Controls the rounding of the corners of the leaf node boxes.
      shadow.col      = 0,     # Color of the shadow under the boxes. Default 0, no shadow.
      prefix          = "",    # Prepend this string to the node labels.
      suffix          = "",    # Append this string to the node labels. Text after a double newline "\n\n" (if any) will be plotted under the box.
      xsep            = NULL,  # String which separates the individual counts and probabilities in node labels when extra>0.
      # Text under the boxes options (only if UNDER=TRUE)
      #under.font = font,
      under.col  = 1,
      under.cex  = 0.8,
      # Split labels options
      split.cex          = ifelse(is.null(CEX), 1, CEX), # default 1
      split.font         = 2,
      #split.family       = family,
      split.col          = 1,
      split.box.col      = 0,
      split.border.col   = 0,
      split.lty          = 1,
      split.lwd          = NULL,
      split.round        = 0,
      split.shadow.col   = 0,
      split.prefix       = "",
      right.split.prefix = NULL,
      split.suffix       = "",
      right.split.suffix = NULL,
      facsep             = ",",
      eq                 = " = ",
      lt                 = " <= ",
      ge                 = " > ",
      # Branch options
      branch.type      = BRANCH.TYPE, # default = 0
      branch.col       = BRANCH.COL,
      branch.lty       = 1,
      branch.lwd       = NULL,
      branch.tweak     = 1,
      min.branch.width = 0.002,
      branch.fill      = BRANCH.COL,
      # Node ID options (only if nn=TRUE)
      nn.cex        = CEX, #default NULL
      nn.font       = 4,
      nn.family     = "",
      nn.col        = cols$textcol,
      nn.box.col    = cols$boxcol, # default 0
      nn.border.col = "black",
      nn.lty        = 1,
      nn.lwd        = NULL,
      nn.round      = 0.3,
      yes.text      = "yes",
      no.text       = "no",
      # User-definable options
      node.fun  = NULL,
      split.fun = NULL,
      FUN = "text",
      # Esoteric parameters, mostly for the graph layout engine.
      nspace         = BRANCH, #(branch)
      minbranch      = 0.3,
      do.par         = TRUE,
      add.labs       = TRUE,
      clip.left.labs = FALSE,
      fam.main       = "",
      yshift         = 0,
      yspace         = SPACE, # (space)
      shadow.offset  = SHADOW.OFFSET,
      # Split options
      split.adj      = NULL,
      split.yshift   = 0,
      split.space    = SPACE, # (space)
      split.yspace   = YSPACE, # (yspace)
      split.shadow.offset = SHADOW.OFFSET,
      # Node ID options
      nn.adj    = 0.5,
      nn.yshift = 0,
      nn.space  = 0.8,
      nn.yspace = 0.5,
      # Other options
      ygap              = GAP/2,
      under.ygap        = 0.5,
      yesno.yshift      = 0,
      ycompact          = UNIFORM,
      xcompact          = TRUE,
      xcompact.ratio    = 0.8,
      min.inter.height  = 4,
      max.auto.cex      = 1,
      min.auto.cex      = 0.15,
      ycompress         = UNIFORM, # If TRUE (the default unless uniform=FALSE), make more space by shifting labels vertically where space is available. Actually, this only kicks in if the initial automatically calculated cex is less than 0.7.
      ycompress.cex     = 0.7,
      accept.cex        = 1.1,
      shift.amounts     = c(1.5, 2),
      Fallen.yspace     = 0.1,
      boxes.include.gap = FALSE,
      # Legend options
      legend.x = NULL,
      legend.y = NULL,
      legend.cex = 1)

  if (!is.null(LEGEND.X) && !is.null(LEGEND.Y)) {
    legend(x=LEGEND.X, y=LEGEND.Y,                    # X and Y position
           ncol   = LEGEND.NCOL,                      # Number of columns
           legend = paste(levels(frp$frame$var)[-1]), # Labels
           fill   = varcols[-1],                      # Filling for boxes
           bg     = "#ffffff66",                      # Background oclour of the legend box
           bty    = "o",                              # Border of legend box
           cex    = LEGEND.CEX * min(1.1 * CEX, 1)    # Scaling factor
           #xpd    = NA                                # Add on top of previous plot
           )
  }

  if (!is.null(FILENAME)) {
    dev.off()
  }

}
