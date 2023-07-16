
get_bioc_refs <- function(pkg) {
  synchronize(async_get_bioc_refs(pkg))
}

async_get_bioc_refs <- function(pkg) {
  url <- bioc_url(pkg)
  asNamespace("pkgdepends")$async_git_dumb_list_refs(url)$
    then(function(ret) order_refs(ret$refs))
}

get_github_refs <- function(pkg) {
  synchronize(async_get_github_refs(pkg))
}

async_get_github_refs <- function(pkg) {
  url <- github_url(pkg)
  asNamespace("pkgdepends")$async_git_list_refs(url)$
    catch(async_http_401 = function(err) {
      list(refs = data.frame(
        stringsAsFactors = FALSE,
        ref = character(),
        hash = character()
      ))
    })$
    then(function(ret) order_refs(ret$refs))
 }

order_refs <- function(x) {
  x <- x[order(x$ref),, drop = FALSE]
  rownames(x) <- NULL
  x
}

get_missing_refs <- function(pkg) {
  synchronize(async_get_missing_refs(pkg))
}

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
