
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
#' @param sleep Amount of seconds to wait after each request.
#' @param saveas If not `NULL` then path to an RDS file, where the results
#' are saved after each package. This is useful to still obtain a partial
#' result if an error happens.
#'
#' @return Named list of data frames. Names are package names, entries
#' are data frames from [get_bioc_refs()].
#'
#' @export
#' @family refs
#' @examplesIf FALSE
#' get_all_github_refs(saveas = "bioc.rds")

get_all_bioc_refs <- function(sleep = 0, saveas = NULL) {
  pkgs <- list_bioc_repos()
  refs <- structure(vector("list", length(pkgs)),  names = pkgs)

  for (i in seq_along(pkgs)) {
    pkg <- pkgs[i]
    done <- format(c(i, length(pkgs)))
    cli::cli_alert_info("[{done[1]}/{done[2]}] {pkg}")
    refs[[i]] <- get_bioc_refs(pkg)
    if (!is.null(saveas)) saveRDS(refs, saveas, version = 2)
    Sys.sleep(sleep)
  }

  refs
}

#' Write thee state of the Bioconductor mirror at Github into files
#'
#' @details
#' See [read_github_data()] for the format.
#'
#' @param state State, as returned by [get_all_github_refs()], a named
#' list of data frames.
#' @param path Path to the root directory of the state files.
#'
#' @export
#' @family refs
#'
#' @examplesIf FALSE
#' state <- get_all_github_refs()
#' write_github_state(state, "../biocatgh-data")

write_github_state <- function(state, path = ".") {
  for (nm in names(state)) {
    refs <- state[[nm]]
    of <- file.path(path, "github-refs", substr(tolower(nm), 1, 2), nm)
    mkdirp(dirname(of))
    write.csv(state[[nm]], file = of, row.names = FALSE)
  }
}

#' Read the state of the Bioconductor mirror at Github, from files
#'
#' @details
#' For each package, the state is in a CSV file with headers, and without
#' row names, at
#' ```
#' github-refs/<prefix>/<package-name>
#' ```
#' where `<refix>` is the two character, lowercase prefix of the package
#' name. E.g.:
#' ```
#' github-refs/a4/a4
#' github-refs/ve/Vega
#' ```
#'
#' @param path Path to the root directory of the state files.
#' @return Named list of data frames, in the format of [get_github_refs()].
#'
#' @export
#' @family refs
#'
#' @examplesIf FALSE
#' read_gituhb_state("../biocatgh-data")

read_github_state <- function(path = ".") {
  files <- dir(file.path(path, "github-refs"), recursive = TRUE, full.names = TRUE)
  refs <- lapply(files, read.csv, stringsAsFactors = FALSE)
  names(refs) <- basename(files)
  refs <- refs[order(tolower(names(refs)))]
  refs
}
