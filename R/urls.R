
bioc_url <- function(pkg) {
  if(pkg == "manifest") {
    return("https://git.bioconductor.org/admin/manifest")
  }
  sprintf(
    "https://git.bioconductor.org/packages/%s",
    pkg
  )
}

github_url <- function(pkg) {
  sprintf(
    "https://github.com/bioc/%s.git",
    pkg
  )
}
