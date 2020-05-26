#' Load the fisheries group file
#'
#' Read in the fisheries group file, which is typically saved as
#' \code{fisheries.csv}.
#'
#' @template dir
#' @template file_fish
#'
#' @author Alexander Keth
#' @export
#' @family load functions
#'
#' @return A \code{data.frame} of functional group information.
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' fleets <- load_fisheries(d, "fisheries.csv")
#' rm(fleets)
#'
load_fisheries <- function(dir = getwd(), file_fish){
  if (is.null(dir)) {
    file.fish <- file_fish
  } else {
    file.fish <- file.path(dir, file_fish)
  }
  result <- read.table(file = file.fish, sep = ",",
                       header = TRUE, stringsAsFactors = FALSE)
  return(result)
}
