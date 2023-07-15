
#' Run a git command using system git
#'
#' System git must be properly installed and it must be in the `PATH`.
#'
#' It throws an error on error.
#'
#' @param ... Arguments to pass to git.
#' @param env Named character vector, environment variables
#'   to set for git.
#' @param timeout Timeout, in seconds, defaults to one hour.
#' @return Return value from [processx::run()].
#'
#' @keywords internal

git <- function(cmd, args = character(), env = character(),
                timeout = 60 * 60, ...) {
  processx::run(
    "git",
    args = c(cmd, args),
    env = c(Sys.getenv(), env),
    timeout = timeout,
    ...
  )
}

largest_git_files <- function(n = 10) {
  files <- git("rev-list", c("--objects", "--all"))$stdout
  tmp <- tempfile("biocatgh-")
  on.exit(unlink(tmp), add = TRUE)
  writeChar(trimws(files), tmp)
  out <- git(
    "cat-file",
    c("--batch-check=%(objecttype) %(objectname) %(objectsize) %(rest)"),
    stdin = tmp
  )

  con <- NULL
  on.exit(tryCatch(close(con), error = function(e) e), add = TRUE)
  files <- read.delim(
    con <- textConnection(out$stdout),
    sep = " ",
    header = FALSE
  )

  # handle empty repos
  if (ncol(files) == 2) {
    files <- data.frame(
      type = character(),
      hash = character(),
      size = character(),
      path = character()
    )
  } else {
    names(files) <- c("type", "hash", "size", "path")
  }

  files <- files[files$type == "blob", ]
  files$size <- as.integer(files$size)
  files <- files[, -1]
  files <- files[order(files$size, decreasing = TRUE),, drop = FALSE]
  rownames(files) <- NULL

  head(files, n)
}
