
#' Download all git refs from the GitHub mirror of Bioconductor
#'
#' @param sleep Amount of seconds to wait after each request.
#' @param saveas If not `NULL` then path to an RDS file, where the results
#' are saved after each package. This is useful to still obtain a partial
#' result if an error happens.
#'
#' @return Named list of data frames. Names are package names, entries
#' are data frames from [get_github_refs()].
#'
#' @export
#' @family refs
#' @examplesIf FALSE
#' get_all_github_refs(saveas = "biocatgh.rds")

get_all_github_refs <- function(sleep = 1, saveas = NULL) {
  pkgs <- list_bioc_repos()
  refs <- structure(vector("list", length(pkgs)),  names = pkgs)

  for (i in seq_along(pkgs)) {
    pkg <- pkgs[i]
    done <- format(c(i, length(pkgs)))
    cli::cli_alert_info("[{done[1]}/{done[2]}] {pkg}")
    refs[[i]] <- get_github_refs(pkg)
    if (!is.null(saveas)) saveRDS(refs, saveas, version = 2)
    Sys.sleep(sleep)
  }

  refs
}

#' Download all git refs from Bioconductor git
#'
#' @param saveas If not `NULL` then path to an RDS file, where the results
#' are saved after each package. This is useful to still obtain a partial
#' result if an error happens.
#' @param parallel_limit The number of git queries to run concurrently.
#'
#' @return Named list of data frames. Names are package names, entries
#' are data frames from [get_bioc_refs()].
#'
#' @export
#' @family refs
#' @examplesIf FALSE
#' get_all_github_refs(saveas = "bioc.rds")

get_all_bioc_refs <- function(saveas = NULL, parallel_limit = 10L) {
  pkgs <- list_bioc_repos()
  refs <- structure(vector("list", length(pkgs)), names = pkgs)
  i <- 0

  synchronize(async_map(pkgs, .limit = parallel_limit, function(pkg) {
    pkg
    async_get_bioc_refs(pkg)$
      then(function(res) {
        refs[[pkg]] <<- res
        i <<- i + 1L
        done <- format(c(i, length(pkgs)))
        cli::cli_alert_info("[{done[1]}/{done[2]}] {pkg}")
        if (!is.null(saveas)) saveRDS(refs, saveas, version = 2)
      })
  }))

  refs
}
