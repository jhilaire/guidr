check_local_version <- function(guide_path) {

  log <- shell(
    paste0("cd ",guide_path," & guide"),
    intern=TRUE
  )

  ver <- strsplit(trimws(log[2]), " ", fixed=TRUE)[[1]][2]

  return(ver)
}
