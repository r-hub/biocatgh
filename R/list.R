
list_bioc_repos <- function() {
  url <- "https://git.bioconductor.org/"
  resp <- curl::curl_fetch_memory(url)
  if (resp$status_code != 200) {
    stop("Cannot get ", url, ", status code ", resp$status_code)
  }
  con <- NULL
  on.exit(tryCatch(close(con), error = function(e) e), add = TRUE)
  txt <- readLines(con <- rawConnection(resp$content))
  txt <- grep("\tpackages/[^.]", txt, value = TRUE)
  pkgs <- sub("^.*\tpackages/", "", txt)

  pkgs[order(tolower(pkgs))]
}