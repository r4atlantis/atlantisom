#' @title Load Atlantis \code{[...]catch.txt} file
#'
#' @description This function loads the Atlantis catch.txt output file.
#'   Where catch is only available for species that are fished
#'   in the \code{functionalGroups.csv} file.
#' @template dir
#' @template file_catch
#' @template verbose
#' @family load functions
#' @return A data frame of total catch weight outputs from the \code{catch.txt} file.
#' @author Sarah Gaichas
#' @export
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file <- "outputSETAScatch.txt"
#' test <- load_catch(dir = d, file_catch = file)
load_catch <- function(dir, file_catch, verbose = FALSE) {
  file.catch <- file.path(dir, file_catch)
  catchbio <- read.table(file.catch, header = TRUE)
  if (verbose) {
    if (abs(recruits[1, 2] / recruits[2, 2]) > 10) {
      warning(paste("The young of the year data you are reading in\n",
        "is from legacy Atlantis code, which had a bug if ran prior\n",
        "to November 2015, where the first row of the young of the year\n",
        "data is in biomass and the rest are in ages.\n",
        "load_catch does not correct for this and post-processing should\n",
        "be done to correct it."))
    }
  }
  return(recruits)
}
