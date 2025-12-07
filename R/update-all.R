
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

update_all <- function(sleep = 5, gh_state = NULL, bioc_state = NULL) {
  gh_state <- gh_state %||% {
    cli::cli_alert_info("Getting all GH refs.")
    get_all_github_refs("gh.rds")
  }
  bioc_state <- bioc_state %||% {
    cli::cli_alert_info("Getting all Bioconductor refs.")
    get_all_bioc_refs("bioc.rds")
  }

  missing <- setdiff(names(bioc_state), names(gh_state))
  common <- intersect(names(bioc_state), names(gh_state))
  gh_state <- gh_state[common]
  bioc_state <- bioc_state[common]

  behind <- vapply(common, function(nm) {
   s <- calculate_state(bioc = bioc_state[[nm]], github = gh_state[[nm]])
   s$needs_update
  }, logical(1))

  upd <- c(missing, names(which(behind)))
  cli::cli_alert_info("Need to update {length(upd)} package{?s}.")

  pkgs <- data.frame(
    package = upd,
    status = NA_character_
  )

  failures <- list()

  for (idx in seq_len(nrow(pkgs))) {
    for (c in 1:3) {
      tryCatch({
        pkgs$status[idx] <- update_package(pkgs$package[idx])
        break
      }, error = function(e) {
        if (c == 3) {
          failures <<- c(failures, structure(list(e), names = pkgs$package[idx]))
          cli::cli_alert_danger(
            "{.pkg {pkgs$package[idx]}} failed to update, see full error at the end."
          )
        } else {
          Sys.sleep(10)
        }
      })
    }
    Sys.sleep(sleep)
  }

  if (length(failures) > 0) {
    cli::cli_alert_danger("Failed to update {length(failures)} package{?s}.")
    for (idx in seq_along(failures)) {
      cli::cli_h2("{names(failures)[idx]}")
      print(failures[[idx]])
    }
    stop("Update failure")
  }

  pkgs
}
