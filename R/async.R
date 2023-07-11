
synchronize <- function(...) {
  asNamespace("pkgdepends")$synchronize(...)
}

when_all <- function(...) {
  asNamespace("pkgdepends")$when_all(...)
}
