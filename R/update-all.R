
#' Update the Bioconductor mirror at GitHub
#'
#' @details
#' The state of the GitHub mirror is obtained from
#' https://github.com/r-hub/biocatgh-data. This data if updated, once all
#' outdated GitHub repositories are updated.
#'
#' The `BIOCATGH_GITHUB_TOKEN` environment variable must be set to the
#' GitHub personal access token to use to update this repository.
#'
#' @param bioc State od the Bioconductor git repository. If `NULL`, then
#'   it is queried using [get_all_bioc_refs()].
#' @param sleep Number of seconds to wait after each update, to please
#'   GitHub secondary rate limits.
#'
#' @export

update_all <- function(bioc = NULL, sleep = 3) {

  if (is.null(bioc)) {
    cli::cli_alert_info("Getting Bioconductor state")
    bioc <- get_all_bioc_refs()
  }

  url <- "https://github.com/r-hub/biocatgh-data"
  user <- "cran-robot"
  token <- Sys.getenv("BIOCATGH_GITHUB_TOKEN", NA_character_)
  if (is.na(token)) {
    stop("Cannot update state at GitHub, no `BIOCATGH_GITHUB_TOKEN` env var.")
  }
  url <- sub("^https://", paste0("https://", user, ":", token, "@"), url)

  tmp <- tempfile()
  dir.create(tmp)
  wd <- getwd()
  on.exit(setwd(wd), add = TRUE)
  setwd(tmp)

  proc <- cli::cli_process_start("Cloning GitHub repo from {.url {safe_url(url)}}")
  git("clone", url)
  setwd("biocatgh-data")
  cli::cli_process_done(proc)

  gh <- read_github_state()

  upd <- character()
  for (nm in names(bioc)) {
    state <- calculate_state(bioc[[nm]], gh[[nm]])
    if (state$needs_update) {
      Sys.sleep(sleep)
      upd <- c(upd, nm)
      update_package(nm, state = state)
      gh[[nm]] <- bioc[[nm]]
    }
  }

  write_github_state(gh)

  if (length(upd)) {
    git("add", "github-refs")
    stat <- git("commit", c("-m", "github ref updates"), error_on_status = FALSE)
    if (stat$status != 0) {
      if (grepl("nothing to commit", stat$stdout)) {
        cli::cli_alert_success("Nothing to update, everything current")
      } else {
        stop("git commit failed:\n", "stdout:\n", stat$stdout,
             "stderr:\n", state$stderr)
      }
    }
    proc <- cli::cli_process_start("Push status cache to GitHub")
    git("push")
    cli::cli_process_done(proc)
  } else {
    cli::cli_alert_success("Nothing to update, everything current")
  }

  invisible()
}
