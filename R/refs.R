
get_repo_tags <- function(pkg) {
  synchronize(async_get_repo_tags(pkg))
}

async_get_repo_tags <- function(pkg) {
  url <- sprintf(
    "https://git.bioconductor.org/packages/%s",
    pkg
  )
  pkgdepends:::async_git_dummy_list_refs(url)$
    then(function(ret) ret$refs)
}
