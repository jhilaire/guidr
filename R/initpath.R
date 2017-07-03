#' Initialise file paths
#'
#' This function initialises file paths.
#' @param outpath
#' @param commonName
#' @keywords GUIDE, file, directory, folder, path
#' @export


initpath <- function(outpath, commonName="GUIDEfile") {

  if (!dir.exists(outpath)) dir.create(outpath)

  v_fname_desc  <- paste0(commonName, "_desc.txt")
  v_fname_data  <- paste0(commonName, "_data.txt")
  v_fname_in    <- paste0(commonName, "_in.txt")
  v_fname_is    <- paste0(commonName, "_isin.txt")
  v_fname_isres <- paste0(commonName, "_isres.txt")
  v_fname_out   <- paste0(commonName, "_out.txt")
  v_fname_sfv   <- paste0(commonName, "_sfv.txt")
  v_fname_rnam  <- paste0(commonName, "_regnames.txt")
  v_fname_rcoef <- paste0(commonName, "_regCoeffs.txt")
  v_fname_fn    <- paste0(commonName, "_fitnod.txt")
  v_fname_tex   <- paste0(commonName, ".tex")
  v_fname_R     <- paste0(commonName, "_predict.R")
  v_fname_pdf   <- paste0(commonName, "_tree.pdf")

  v_fpath_desc  <- file.path(outpath, v_fname_desc)
  v_fpath_data  <- file.path(outpath, v_fname_data)
  v_fpath_in    <- file.path(outpath, v_fname_in)
  v_fpath_is    <- file.path(outpath, v_fname_is)
  v_fpath_isres <- file.path(outpath, v_fname_isres)
  v_fpath_out   <- file.path(outpath, v_fname_out)
  v_fpath_sfv   <- file.path(outpath, v_fname_sfv)
  v_fpath_rnam  <- file.path(outpath, v_fname_rnam)
  v_fpath_rcoef <- file.path(outpath, v_fname_rcoef)
  v_fpath_fn    <- file.path(outpath, v_fname_fn)
  v_fpath_tex   <- file.path(outpath, v_fname_tex)
  v_fpath_R     <- file.path(outpath, v_fname_R)
  v_fpath_pdf   <- file.path(outpath, v_fname_pdf)

  v_fpath            <- list()
  v_fpath[["in"]]    <- v_fpath_in
  v_fpath[["is"]]    <- v_fpath_is
  v_fpath[["isres"]] <- v_fpath_isres
  v_fpath$out        <- v_fpath_out
  v_fpath$desc       <- v_fpath_desc
  v_fpath$data       <- v_fpath_data
  v_fpath$tex        <- v_fpath_tex
  v_fpath$sfv        <- v_fpath_sfv
  v_fpath$rnam       <- v_fpath_rnam
  v_fpath$rcoef      <- v_fpath_rcoef
  v_fpath$fn         <- v_fpath_fn
  v_fpath$R          <- v_fpath_R
  v_fpath$outPath    <- outpath

  out <- v_fpath

  return(out)
}
