
update_package <- function(pkg, state = NULL, force = FALSE) {
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

  # git("remote", c("add", "github", ugithub))
  git("remote", c("add", "github", paste0("git@github.com:bioc/", pkg, ".git")))

  # Need to push devel first to make it the default branch
  # Ignore the errors
  tryCatch({
    git("push", c("-q", "github", "devel"))
    Sys.sleep(1)
    git("push", c("-q", "github", "master"))
    Sys.sleep(1)
    git("push", c("-q", "github", "main"))
  }, error = function(e) e)

  # Need to remove these refs to avoid pushing them to GH
  unlink("refs/remotes", recursive = TRUE)

  Sys.sleep(1)
  git("push", c("-q", "github", "--mirror"))

  cli::cli_alert_success("{.pkg {pkg}} was updated")
  invisible("updated")
}
