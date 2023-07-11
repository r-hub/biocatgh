
create_repo <- function(pkg) {
  cli::cli_alert_info("Creating repo for {.pkg {pkg}}")
  tryCatch(
    gh::gh(
      "POST /orgs/:org/repos",
      org = "bioc",
      name = pkg,
      description = paste(
        "This is a read-only mirror of the git repos at",
        "https://bioconductor.org"
      )
    ),
    http_error_422 = function(err) {
      cli::cli_alert_info(
        "Cannot create {.pkg {pkg}} repo, it already exists?"
      )
    },
    error = function(err) {
      browser()
      stop(err)
    }
  )
}
