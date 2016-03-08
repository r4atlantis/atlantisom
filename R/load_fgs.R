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
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' fgs <- load_fgs(d, "functionalGroups.csv")
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
