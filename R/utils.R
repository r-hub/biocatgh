
`%||%` <- function(l, r) if (is.null(l)) r else l

split_lines <- function(x) {
  if (length(x) != 1) {
    stop("split_lines() only works on scalars")
  }
  strsplit(x, "\n", fixed = TRUE)[[1]]
}
