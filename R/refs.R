
#' Get all git refs of a package from Bioconductor git
#'
#' @details
#' It errors if the repository does not exist or on any git error.
#'
#' It uses the dummy git protcol.
#'
#' @param pkg Package name.
#' @return A data frame with columns:
#' * `ref`: git ref name, the first one is `HEAD`, if the repository is
#'   not empty.
#' * `hash`: SHA-1 hash.
#'
#' @export
#' @family refs
#' @examplesIf FALSE
#' get_bioc_refs("limma")

get_bioc_refs <- function(pkg) {
  synchronize(async_get_bioc_refs(pkg))
}

#' @export
#' @rdname get_bioc_refs
#' @details
#' `async_get_bioc_refs()` is the aynchronous version of `get_bioc_refs()`.

async_get_bioc_refs <- function(pkg) {
  url <- bioc_url(pkg)
  asNamespace("pkgdepends")$async_git_dumb_list_refs(url)$
    then(function(ret) order_refs(ret$refs))
}

#' Get all git refs from the GitHub Bioconductor mirror
#'
#' @details
#' It the repository does not exist, it'll return an empty data frame.
#'
#' @param pkg Package name.
#' @return A data frame with columns:
#' * `ref`: git ref name, the first one is `HEAD`, if the repository exists
#'   and it is not empty.
#' * `hash`: SHA-1 hash.
#'
#' @export
#' @family refs
#' @examplesIf FALSE
#' get_github_refs("limma")

get_github_refs <- function(pkg) {
  synchronize(async_get_github_refs(pkg))
}

#' @export
#' @rdname get_github_refs
#' @details
#' `async_get_github_refs()` is the aynchronous version of
#' `get_github_refs()`.

async_get_github_refs <- function(pkg) {
  url <- github_url(pkg)

  empty_df <- function() {
    list(refs = data.frame(
      stringsAsFactors = FALSE,
      ref = character(),
      hash = character()
    ))
  }

  asNamespace("pkgdepends")$async_git_list_refs(url)$
    catch(async_http_401 = function(err) empty_df())$
    catch(async_http_404 = function(err) empty_df())$
    catch(error = function(err) {
      cli::cli_alert_warning("Failed to get {.url {url}}, retrying after delay.")
      Sys.sleep(30)
      async_git_list_refs(url)
    })$
    then(function(ret) order_refs(ret$refs))
 }

order_refs <- function(x) {
  x <- x[order(x$ref),, drop = FALSE]
  rownames(x) <- NULL
  x
}

#' Get git refs that are missing from or are different in the GitHub mirror
#'
#' Also report extra refs, and whether the repository exists.
#'
#' @param pkg Package name.
#' @return A list with entries:
#' * `missing`: character vector of refs missing from GitHub.
#' * `extra`: character vector of extra refs on GitHub.
#' * `different`: character vecotr of refs that have a different hash at
#'   GitHub.
#' * `repo_exists`: logical scalar, whether the repository exists and it is
#'   non-empty on GitHub.
#' * `needs_update`: whether the repository needs an update. This is
#'   calculated from the other fields.
#'
#' @export
#' @family refs
#' @examplesIf FALSE
#' get_missing_refs("limma")

get_missing_refs <- function(pkg) {
  synchronize(async_get_missing_refs(pkg))
}

#' @export
#' @rdname get_missing_refs
#' @details
#' `async_get_missing_refs()` is the aynchronous version of
#' `get_missing_refs()`.

async_get_missing_refs <- function(pkg) {
  when_all(
    bioc = async_get_bioc_refs(pkg),
    github = async_get_github_refs(pkg)
  )$
  then(function(state) {
    calculate_state(github = state$github, bioc = state$bioc)
  })
}

calculate_state <- function(bioc, github) {
  github <- github %||% data.frame(ref = character(), hash = character())
  gh_exists <- nrow(github) > 0
  bc_exists <- nrow(bioc) > 0
  extra <- setdiff(github$ref, bioc$ref)
  miss <- setdiff(bioc$ref, github$ref)

  biocrefs <- structure(bioc$hash, names = bioc$ref)
  githubrefs <- structure(github$hash, names = github$ref)

  common <- intersect(names(biocrefs), names(githubrefs))
  biocrefs <- biocrefs[common]
  githubrefs <- githubrefs[common]

  diff <- common[biocrefs != githubrefs]

  update <- (bc_exists && !gh_exists) || length(miss) || length(extra) || length(diff)
  ret <- list(
    missing = miss,
    extra = extra,
    different = diff,
    repo_exists = gh_exists,
    needs_update = update
  )

  ret
}
