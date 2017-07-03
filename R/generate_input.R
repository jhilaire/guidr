#' Generate GUIDE input data
#'
#' This function generates the data and other files required to run GUIDE
#' @param i_data
#' @param i_vardef
#' @param i_treeType
#' @param i_treeOptions
#' @param i_fpath
#' @param VERBOSE=FALSE
#' @keywords GUIDE, input, description file, data file
#' @export
#'

generate_input <- function(i_data, i_vardef, i_treeType, i_treeOptions, i_fpath, i_missVal="NaN", i_startLine="2", i_idcolumns=c("iso", "year"), VERBOSE=FALSE) {
  if (VERBOSE) cat(paste0("  - Removing iso variable to avoid problems with GUIDE...\n"))
  i_data <- i_data[,names(i_data)[which(!names(i_data) %in% i_idcolumns)]]

  #-- Write description file ---------------
  if (VERBOSE) cat(paste0("  - Description file ", file.path(i_fpath$desc),"\n"))
  catret(basename(i_fpath$data), file=file.path(i_fpath$desc))             # Name of the training sample file (relative or absolute path)
  catret(i_missVal,    file=file.path(i_fpath$des), append=T)   # Missing value code (must always be provided)
  catret(i_startLine,  file=file.path(i_fpath$desc), append=T)   # Line number of the first data record in the data file
  # Loop over variables and define column number, name and type
  for (kvar in names(i_data)) {
    catret(paste(
      which(names(i_data) == kvar),                         # Column number
      kvar,                                                 # Variable name
      i_vardef$type[which(i_vardef$variable == kvar)]),     # Variable type
      file=file.path(i_fpath$desc), append=T)
  }

  #-- Write data file ---------------------
  if (VERBOSE) cat(paste0("  - data file ", file.path(i_fpath$data),"\n"))
  write.table(i_data, file=file.path(i_fpath$data), quote=FALSE, na=i_missVal, sep="\t", row.names=FALSE)

  #-- Write batch input file --------------
  if (VERBOSE) cat(paste0("  - Batch input file ", file.path(i_fpath[["in"]]),"\n"))
  if (i_treeType == "Single tree > LMS - Constant") {
    v_singleTree = TRUE
    v_treeType   = "LMS-constant"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > LMS - Multilinear") {
    v_singleTree = TRUE
    v_treeType   = "LMS-multilinear"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > LMS - Best simple linear") {
    v_singleTree = TRUE
    v_treeType   = "LMS-bestSimpleLinear"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > LS - Constant") {
    v_singleTree = TRUE
    v_treeType   = "LS-constant"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > LS - Best polynomial") {
    v_singleTree = TRUE
    v_treeType   = "LS-bestPolynomial"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > LS - Linear") {
    v_singleTree = TRUE
    v_treeType   = "LS-linear"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > LS - Multilinear") {
    v_singleTree = TRUE
    v_treeType   = "LS-multilinear"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > LS - Simple ANCOVA") {
    v_singleTree = TRUE
    v_treeType   = "LS-simpleANCOVA"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > Q1 - Constant") {
    v_singleTree = TRUE
    v_treeType   = "Q1-Constant"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    #write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > Q2 - Constant") {
    v_singleTree = TRUE
    v_treeType   = "Q2-Constant"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    #write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > Q1 - Multilinear") {
    v_singleTree = TRUE
    v_treeType   = "Q1-multiLinear"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    #write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > Q2 - Multilinear") {
    v_singleTree = TRUE
    v_treeType   = "Q2-multiLinear"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    #write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > Q1 - Best Polynomial") {
    v_singleTree = TRUE
    v_treeType   = "Q1-bestPolynomial"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    #write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > Q2 - Best Polynomial") {
    v_singleTree = TRUE
    v_treeType   = "Q2-bestPolynomial"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    #write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > Longitudinal - Lowess smoothing") {
    v_singleTree = TRUE
    v_treeType   = "Longitudinal-Lowess"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    #write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Single tree > Longitudinal - Spline smoothing") {
    v_singleTree = TRUE
    v_treeType   = "Longitudinal-Spline"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
    #write_is_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Tree ensemble > Bagging") {
    v_singleTree = FALSE
    v_treeType   = "Bagging"
    #write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Tree ensemble > Random Forest - Random") {
    v_singleTree = FALSE
    v_treeType   = "RF-random"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  if (i_treeType == "Tree ensemble > Random Forest - non-random") {
    v_singleTree = FALSE
    v_treeType   = "RF-nonRandom"
    write_batch_file(v_singleTree, v_treeType, i_treeOptions, i_fpath, VERBOSE=VERBOSE)
  }
  return(i_data)
}
