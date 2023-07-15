
get_all_github_refs <- function(sleep = 1, saveas = NULL) {
  pkgs <- list_bioc_repos()
  refs <- structure(vector("list", length(pkgs)),  names = pkgs)

  for (i in seq_along(pkgs)) {
    pkg <- pkgs[i]
    done <- format(c(i, length(pkgs)))
    cli::cli_alert_info("[{done[1]}/{done[2]}] {pkg}")
    refs[[i]] <- get_github_refs(pkg)
    if (!is.null(saveas)) saveRDS(refs, saveas, version = 2)
    Sys.sleep(sleep)
  }

  refs
}

get_all_bioc_refs <- function(sleep = 0, saveas = NULL) {
  pkgs <- list_bioc_repos()
  refs <- structure(vector("list", length(pkgs)),  names = pkgs)

  for (i in seq_along(pkgs)) {
    pkg <- pkgs[i]
    done <- format(c(i, length(pkgs)))
    cli::cli_alert_info("[{done[1]}/{done[2]}] {pkg}")
    refs[[i]] <- get_bioc_refs(pkg)
    if (!is.null(saveas)) saveRDS(refs, saveas, version = 2)
    Sys.sleep(sleep)
  }

  refs
}

write_github_state <- function(state, path = ".") {
  for (nm in names(state)) {
    refs <- state[[nm]]
    of <- file.path(path, "github-refs", substr(tolower(nm), 1, 2), nm)
    mkdirp(dirname(of))
    write.csv(state[[nm]], file = of, row.names = FALSE)
  }
}

read_github_state <- function(path = ".") {
  files <- dir(file.path(path, "github-refs"), recursive = TRUE, full.names = TRUE)
  refs <- lapply(files, read.csv, stringsAsFactors = FALSE)
  names(refs) <- basename(files)
  refs <- refs[order(tolower(names(refs)))]
  refs
}
