
#' Update the Bioconductor mirror at GitHub
#'
#' The `BIOCATGH_GITHUB_TOKEN` environment variable must be set to the
#' GitHub personal access token to use to update this repository.
#'
#' @param sleep Number of seconds to wait after each update, to please
#'   GitHub secondary rate limits, and be gentle on the Bioconductor git
#'   server.
#'
#' @export

update_all <- function(sleep = 5) {
  pkgs_url <- "https://code.bioconductor.org/browse/packages.json"
  pkgs_raw <- jsonlite::fromJSON(pkgs_url)[["data"]]
  pkgs <- data.frame(
    stringsAsFactors = FALSE,
    package = unhtml(pkgs_raw[,1]),
    date = parse_date(sub("^([-0-9]+ [0-9:]+ [A-Z]+) .*$", "\\1", pkgs_raw[,2])),
    status = NA_character_
  )
  pkgs <- pkgs[order(pkgs$date, decreasing = TRUE), ]

  for (idx in seq_along(pkgs$package)) {
    pkgs$status[idx] <- update_package(pkgs$package[idx])
    if (pkgs$status[idx] == "current") break
    Sys.sleep(sleep)
  }

  invisible(pkgs)
}

unhtml <- function(x) {
  x <- gsub("<[^>]*>", "", x)
  x <- gsub("&[A-Za-z0-9]+;", "", x)
  x
}

parse_date <- function(x) {
  x <- sub(" ", "T", x)
  x <- sub(" UTC", "Z", x)
  parsedate::parse_iso_8601(x)
}
