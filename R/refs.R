
get_bioc_refs <- function(pkg) {
  synchronize(async_get_bioc_refs(pkg))
}

async_get_bioc_refs <- function(pkg) {
  url <- bioc_url(pkg)
  asNamespace("pkgdepends")$async_git_dummy_list_refs(url)$
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
      exists <- nrow(state$github) > 0
      extra <- setdiff(state$github$ref, state$bioc$ref)
      miss <- setdiff(state$bioc$ref, state$github$ref)

      bioc <- structure(state$bioc$hash, names = state$bioc$ref)
      github <- structure(state$github$hash, names = state$github$ref)

      common <- intersect(names(bioc), names(github))
      bioc <- bioc[common]
      github <- github[common]

      diff <- common[bioc != github]

      update <- !exists || length(miss) || length(extra) || length(diff)
      ret <- list(
        missing = miss,
        extra = extra,
        different = diff,
        repo_exists = exists,
        needs_update = update
      )
    })
}
