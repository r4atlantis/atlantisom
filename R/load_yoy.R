#' @title Load Atlantis \code{[...]YOY.txt} file
#'
#' @description This function loads the Atlantis young of the year file.
#'   Where young of the year is only available for species that are turned
#'   on in the \code{functionalGroups.csv} file.
#' @template dir
#' @template file_yoytxt
#' @family load functions
#' @return A data frame of recruitment outputs from the \code{YOY.txt} file.
#' @author Emma E Hodgson
#' @export
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file <- "outputSETASYOY.txt"
#' test <- load_yoy(dir = d, file_yoy = file)
load_yoy <- function(dir, file_yoy) {
  file.yoy <- file.path(dir, file_yoy)
  recruits <- read.table(file.yoy, header = TRUE)
  return(recruits)
}
