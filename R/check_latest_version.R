check_latest_version <- function(DEBUG=FALSE) {

  fname = paste0(".tmp_GUIDEversion_", format(Sys.time(), "%Y-%m-%d"))

  # Check if website has already been visited today
  if (!file.exists(fname)) {

    # download html
    html <- getURL("http://www.stat.wisc.edu/~loh/guide.html", followlocation = TRUE)

    # parse html
    doc = htmlParse(html, asText=TRUE)
    plain.text <- xpathSApply(doc, "//div[@id='content']/h1[@align='center']/strong", xmlValue)

    # Get version number
    ver <- gsub("version ", "",
                regmatches(
                  paste(plain.text),
                  gregexpr("(?<=\\().*?(?=\\))",
                           paste(plain.text),
                           perl=T))[[1]])

    cat(ver, file = fname, append = FALSE)

    if (DEBUG) paste0("[DEBUG] Get version from website: ", ver, " (save in ", fname, ")")

  } else {

    other_files <- list.files(".", pattern=".tmp_GUIDEversion_", all.files = TRUE)[which(list.files(".", pattern=".tmp_GUIDEversion_", all.files = TRUE) != fname)]
    if (length(other_files) != 0) file.remove(other_files)

    ver <- readLines(fname,n=1)

    if (DEBUG) paste0("[DEBUG] Get version from saved file (", fname,"): ", ver)
  }
  return(ver)

}
