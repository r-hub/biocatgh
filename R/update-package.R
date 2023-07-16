
#' Update one package at the Bioconductor GitHub mirror
#'
#' @param pkg Package name.
#' @param state `NULL`, or the return value of [get_missing_refs()] for
#'   the package. If it is `NULL`, and it is needed (i.e. `force` is not
#'   `TRUE`), then it is calculated.
#' @param force `TRUE` or `FALSE`, whether to always (try to) update the
#'   package withour checking the git refs in both repositories first.
#'   Set this to `TRUE` if you know that an update is needed, to avoid
#'   extra git queries.
#' @param sleep Number of seconds to wait after GitHub git operations,
#'   to please GitHub secondary rate limits.
#'
#' @return Character scalar, possible values:
#'   * `"current"`: no updates are needed.
#'   * `"large-files"`: the repository contains large files, no update was
#'     attempted.
#'   * `"updated"`: the repository was updated.
#'
#' @export
#' @examplesIf FALSE
#' update_package("limma")

update_package <- function(pkg, state = NULL, force = FALSE, sleep = 1) {
  if (!force) {
    state <- state %||% get_missing_refs(pkg)
    if (!state$needs_update) {
      cli::cli_alert_success("{.pkg {pkg}} is current")
      return(invisible("current"))
    }
  }

  cli::cli_alert_info("Updating {.pkg {pkg}}")
  ubioc <- bioc_url(pkg)
  ugithub <- github_url(pkg)

  pkgdir <- withr::local_tempdir("biocatgh")
  withr::local_dir(pkgdir)

  git("clone", c("--mirror", ubioc, pkg))
  setwd(pkg)

  largest_file <- largest_git_files()$size[1]
  if (!is.na(largest_file) && largest_file > 100 * 1024 * 1024) {
    cli::cli_alert_danger("{.pkg {pkg}} has large files, giving up")
    return(invisible("large-files"))
  }

  if (!force) {
    if (!state$repo_exists) {
      create_repo(pkg)
    }
  } else {
    create_repo(pkg)
  }

  git("remote", c("add", "github", ugithub))
  # git("remote", c("add", "github", paste0("git@github.com:bioc/", pkg, ".git")))

  # Need to push devel first to make it the default branch
  # Ignore the errors
  tryCatch({
    git("push", c("-q", "github", "devel"))
    Sys.sleep(sleep)
    git("push", c("-q", "github", "master"))
    Sys.sleep(sleep)
    git("push", c("-q", "github", "main"))
  }, error = function(e) e)

  # Need to remove these refs to avoid pushing them to GH
  unlink("refs/remotes", recursive = TRUE)

  Sys.sleep(sleep)
  git("push", c("-q", "github", "--mirror"))

  cli::cli_alert_success("{.pkg {pkg}} was updated")
  invisible("updated")
}
