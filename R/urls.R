
bioc_url <- function(pkg) {
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
