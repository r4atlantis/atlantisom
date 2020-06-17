#' Write metadata to the disk.
#'
#' @template meta
#' @template dir
#' @template file_out
#' @template verbose
#'
#' @author Kelli Faye Johnson
#' @family write functions
#' @return Write a file to the disk of metadata pertaining to a single Atlantis scenario.
#' @seealso \code{\link{load_meta}}
#'
#' @export
#'
#' @examples
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' meta <- load_meta(d, "outputs")
#' write_meta(meta, dir = NULL, "test.txt")
#' unlink("test.txt")
#' rm(meta)
#'
write_meta <- function(meta, dir = getwd(), file_out = "atlantisom_log.txt",
  verbose = FALSE) {
  # Print the log information to a text file
  if (is.null(dir)) {
    file.out <- file_out
  } else {
    file.out <- file.path(dir, file_out)
  }
  if (verbose) message("Writing the output from load_meta to:\n", file.out)
  sink(file.out)
  on.exit(sink())
  cat("# Meta data from atlantisom\n")
  cat(paste("# Written by load_meta on", Sys.time(), "\n"))
  cat(paste("# by", Sys.info()["user"], "on", Sys.info()["sysname"], "\n"))
  cat(paste("# using", version$version.string, "\n"))
  for (x in seq_along(meta)) {
    cat(paste0("#", names(meta)[x], "\n"))
    cat(meta[[x]])
    cat("\n")
  }
}
