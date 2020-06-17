#' @title Load Atlantis \code{[...]YOY.txt} file
#'
#' @description This function loads the Atlantis young of the year file.
#'   Where young of the year is only available for species that are turned
#'   on in the \code{functionalGroups.csv} file.
#' @template dir
#' @template file_yoy
#' @template verbose
#' @family load functions
#' @return A data frame of recruitment outputs from the \code{YOY.txt} file.
#' @author Emma E Hodgson
#' @export
#'
#' @examples
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' file <- "outputsYOY.txt"
#' test <- load_yoy(dir = d, file_yoy = file)
load_yoy <- function(dir, file_yoy, verbose = FALSE) {
  file.yoy <- file.path(dir, file_yoy)
  recruits <- read.table(file.yoy, header = TRUE)
  if (verbose) {
    if (abs(recruits[1, 2] / recruits[2, 2]) > 10) {
      warning(paste("The young of the year data you are reading in\n",
        "is from legacy Atlantis code, which had a bug if ran prior\n",
        "to November 2015, where the first row of the young of the year\n",
        "data is in biomass and the rest are in ages.\n",
        "load_yoy does not correct for this and post-processing should\n",
        "be done to correct it."))
    }
  }
  return(recruits)
}
