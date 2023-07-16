
# We know that these repos have large files

ignored <- c(
  "Affyhgu133aExpr",                 # old annotation? large files
  "Affyhgu133Plus2Expr",             # old annotation? large files
  "Affymoe4302Expr",                 # old annotation? large files
  "ccdata",                          # experiment, large files
  "ChAMPdata",                       # experiment, large files
  "ChIPXpressData",                  # experiment, large files
  "ConnectivityMap",                 # experiment, large files
  "CopyNumber450kData",              # experiment, large files
  "curatedBreastData",               # experiment, large files
  "ELMER.data",                      # experiment, big files, git fails
  "davidTiling",                     # experiment, large files
  "dsQTL",                           # old experiment, large files
  "Fletcher2013b",                   # experiment, large files
  "FlowSorted.Blood.450k",           # experiment, large files
  "FlowSorted.CordBlood.450k",       # experiment, large files
  "FlowSorted.CordBloodNorway.450k", # experiment, large files
  "FlowSorted.DLPFC.450k",           # experiment, large files
  "furrowSeg",                       # experiment, large files
  "GeuvadisTranscriptExpr",          # experiment, large filse
  "h5vc",                            # has large files in history, to be fixed
  "hapmapsnp6",                      # experiment, large filse
  "HD2013SGI",                       # experiment, large files
  "Hiiragi2013",                     # experiment, large files
  "ListerEtAlBSseq",                 # experiment, large files
  "mammaPrintData",
  "MEALData",
  "methylationArrayAnalysis",
  "MMDiffBamSubset",
  "msdata",
  "msPurityData",
  "oneChannelGUI",
  "pd.atdschip.tiling",
  "proteomics",
  "RforProteomics",
  "RnBeads.hg19",
  "SCATEData",
  "sciCNV",
  "Single.mTEC.Transcriptomes",
  "SVM2CRMdata",
  "SwathXtend",
  "waveTilingData",
  "yriMulti",
  NULL
)

#' List all packages in the Bioconductor git repository
#'
#' @details
#' It works by web scraping https://git.bioconductor.org, so it'll probably
#' stop working if Bioconductor changes their git infrastructure.
#'
#' It skips packages that are known to e problematic, typically because
#' of files bigger than 100MB. These files are not allowed on GitHub
#'
#' @return
#' Sorted character vector of package (=repository) names.
#'
#' @export

list_bioc_repos <- function() {
  url <- "https://git.bioconductor.org/"
  resp <- curl::curl_fetch_memory(url)
  if (resp$status_code != 200) {
    stop("Cannot get ", url, ", status code ", resp$status_code)
  }
  con <- NULL
  on.exit(tryCatch(close(con), error = function(e) e), add = TRUE)
  txt <- readLines(con <- rawConnection(resp$content))
  txt <- grep("\tpackages/[^.]", txt, value = TRUE)
  pkgs <- sub("^.*\tpackages/", "", txt)

  pkgs <- setdiff(pkgs, ignored)

  pkgs[order(tolower(pkgs))]
}
