#' Load the functional group file
#'
#' Read in the functional group file, which is typically saved as
#' \code{functionalGroups.csv}.
#'
#' @template dir
#' @template file_fgs
#'
#' @author Alexander Keth
#' @export
#' @family load functions
#'
#' @return A \code{data.frame} of functional group information.
#'
#' @examples
#' testscenario <- "INIT_VMPA_Jan2015"
#' d <- system.file("extdata", testscenario, package = "atlantisom")
#' file <- "functionalGroups.csv"
#' fgs <- load_fgs(d, file)
#' rm(fgs)
#'
load_fgs <- function(dir = getwd(), file_fgs) {
  if (is.null(dir)) {
    file.fgs <- file_fgs
  } else {
    file.fgs <- file.path(dir, file_fgs)
  }
  result <- read.table(file = file.fgs, sep = ",",
    header = TRUE, stringsAsFactors = FALSE)
  return(result)
}
