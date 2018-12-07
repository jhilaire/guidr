# guidr
An R library to allow bilateral communication with GUIDE

GUIDE Classification and Regression Trees and Forests.

"GUIDE is a multi-purpose machine learning algorithm for constructing classification and regression trees. It is designed and maintained by Wei-Yin Loh at the University of Wisconsin, Madison. GUIDE stands for Generalized, Unbiased, Interaction Detection and Estimation."

More information is available on the website: http://www.stat.wisc.edu/~loh/guide.html

Warning: Only a few models are available at the moment.

# Install
devtools::install_github("jhilaire/guidr")

# Use
```
library(guidr)
library(dplyr)

#-- User section -----------------------------------
u_NbData    <- 1000
u_threshold <- 5000
u_range     <- c(0, 10000)

#-- Generate data ----------------------------------
x1 <- runif(u_NbData, min = u_range[1],  max=u_threshold)
x2 <- runif(u_NbData, min = u_threshold, max=u_range[2])
mydata <- data.frame(
  x = c(x1, x2),
  y = c(
     2*x1 + rnorm(u_NbData)*500,
    -2*x2 + rnorm(u_NbData)*500 + 4*u_threshold)
)
plot(data$x, data$y)

#-- Initialise GUIDE file paths and tree options ---
outPath  <- "test"
treePath <- initpath(outPath)
dir.create(treePath$outPath, recursive = TRUE)

#-- Set tree options -------------------------------
treeOpts <- initTreeOptions(prune      = "1",  # 1=prune by CV, 2=no pruning
                            nbcv       = 10,   # Number of cross-validation steps (i.e. number of sub data spaces)
                            cvtype     = "1",  # 1=mean-based CV tree, 2=median-based CV tree
                            sevalue    = 0.5,  # Standard Error number for pruning [0..1]
                            search     = "2",  # 1=split point from quantiles, 2=use exhaustive search
                            splitfrac  = NA,   # 1=accept default splitting fraction (i.e. frac, where #splits = max(9,fract*n), with n = #cases in node), 2=change it
                            truncmeth  = "3",  # Data truncation method (0=none, 1=node range, 2=+10% node range, 3=global range, 4=2-sided Winsorization)
                            missregval = "2",  # missing regressor values: 1=separate models, 2=impute with means, 3=constant model
                            maxsplits  = 10,   # max. no. split levels
                            minnbnodes = 2)    # min. node sample size 

#-- Set variable definition ------------------------
variableDefinition <- data.frame(
  variable = c("x", "y", "z"),
  type     = c("n", "d", "c")
)

#-- Generate GUIDE input data ----------------------
g_data <- generate_input(mydata, variableDefinition, 
                         i_treeType    = "Single tree > LMS - Multilinear", 
                         i_treeOptions = treeOpts, 
                         i_fpath       = treePath)
                                           
#-- Run GUIDE --------------------------------------
run_guide(treePath)

#-- Parse results ----------------------------------
g_res   <- parse_output(file.path(outPath, "GUIDEfile_out.txt"), variableDefinition)
  
#-- Plot results -----------------------------------
tree <- as.fake.rpart.tree(g_res)
plot_tree(tree, 
          TITLE=paste0("RT - \nLMS-Multilinear - Tn:", length(which(g_res$nodeType == "Terminal node"))), 
          CEX=NULL, LEGEND.X = 0, LEGEND.Y = 1.1, LEGEND.CEX=0.5, LEGEND.NCOL=2)
          
```
