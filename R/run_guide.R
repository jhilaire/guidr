#' run GUIDE
#'
#' This function calls GUIDE with a provided batch input file.
#' @param i_path
#' @param SHOW_LOG
#' @param DEBUG
#' @keywords GUIDE, run
#' @export


run_guide <- function(i_fpath, IMPORTANCE_SCORING=FALSE, SHOW_LOG=FALSE, DEBUG=FALSE) {

  #tmp <- strsplit(strsplit(basename(i_fpath[["in"]]), ".", fixed=TRUE)[[1]][1], "_", fixed=TRUE)[[1]]
  #bname <- paste0(tmp[seq(1,length(tmp))], collapse="_")
  bname <- strsplit(basename(i_fpath[["in"]]), ".", fixed=TRUE)[[1]][1]

  if (DEBUG) paste0("Running following command: cd ", normalizePath(i_fpath$outPath), " & guide < ", basename(i_fpath[["in"]]))

  if (SHOW_LOG) cat("===================================== START OF GUIDE LOG FILE =========================================\n")
  if (paste(Sys.info()['sysname']) == "Windows") {

    file.copy("bin/guide.exe", i_fpath$outPath)

    # Importance scoring
    if (IMPORTANCE_SCORING) {
      log <- shell(
        paste0("cd ", normalizePath(i_fpath$outPath), " & guide < ", basename(i_fpath[["is"]])),
        intern=TRUE, minimized = FALSE, invisible = FALSE
      )
      if (SHOW_LOG) cat(sapply(log, function(k) paste0(k, "\n")))
    }

    # Regression tree
    log <- shell(
      paste0("cd ", normalizePath(i_fpath$outPath), " & guide < ", basename(i_fpath[["in"]])),
      intern=TRUE, minimized = FALSE, invisible = FALSE
    )
    if (SHOW_LOG) cat(sapply(log, function(k) paste0(k, "\n")))

    writeLines(log, file.path(i_fpath$outPath, paste0(bname, ".log")))
    # file.copy(i_fpath$out, file.path(i_fpath$out))
    # file.copy(i_fpath$fn, file.path(i_fpath$fn))

    file.remove(file.path(i_fpath$outPath, "guide.exe"))
  }
  if (paste(Sys.info()['sysname']) == "Linux") {

    file.copy("bin/guide", i_fpath$outPath)

    # Importance scoring
    if (IMPORTANCE_SCORING) {
      log <- system(
        paste0("cd ", normalizePath(i_fpath$outPath), "; guide < ", basename(i_fpath[["is"]])),
        intern=TRUE
      )
      if (SHOW_LOG) cat(sapply(log, function(k) paste0(k, "\n")))
    }

    # Regression tree
    log <- system(
      paste0("cd ", normalizePath(i_fpath$outPath), "; guide < ", basename(i_fpath[["in"]])),
      intern=TRUE
    )
    if (SHOW_LOG) cat(sapply(log, function(k) paste0(k, "\n")))

    writeLines(log, file.path(i_fpath$outPath, paste0(bname, ".log")))
    # file.copy(i_fpath$out, file.path(i_fpath$out))
    # file.copy(i_fpath$fn, file.path(i_fpath$fn))

    file.remove(file.path(i_fpath$outPath, "guide"))
  }
  if (!paste(Sys.info()['sysname']) %in% c("Windows", "Linux")) {
    stop("Other OS not yet supported.")
  }
  if (SHOW_LOG) cat("====================================== END OF GUIDE LOG FILE =========================================\n\n")
}

# system(command, intern = FALSE, wait = TRUE, input = NULL,
#        show.output.on.console = FALSE,
#        minimized = FALSE, invisible = FALSE)
# Arguments
#
# command	the system command to be invoked, as a string.
# intern	a logical, indicates whether to make the output of the command an R object.
# wait	should the R interpreter wait for the command to finish? The default is to wait, and the interpreter will always wait if intern = TRUE.
# input	if a character vector is supplied, this is copied one string per line to a temporary file, and the standard input of command is redirected to the file.
# show.output.on.console	a logical, indicates whether to capture the output of the command and show it on the R console (not used by Rterm, which captures the output unless wait is false).
# minimized	a logical, indicates whether the command window should be initially displayed as a minimized window.
# invisible	a logical, indicates whether the command window should be visible on the screen.
