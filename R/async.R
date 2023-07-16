
synchronize <- function(...) {
  asNamespace("pkgdepends")$synchronize(...)
}

when_all <- function(...) {
  asNamespace("pkgdepends")$when_all(...)
}

async_map <- function(...) {
  asNamespace("pkgdepends")$async_map(...)
}
