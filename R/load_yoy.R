#' Load Atlantis \code{YOY.txt} file
#'
#' This function loads the Atlantis biology parameter output file.
#' @template dir
#' @template file_yoytxt, for example "outputSETASYOY.txt"
#' @family load functions
#' @return A data frame of recruitment outputs from the \code{YOY.txt} file.
#' @author Emma E Hodgson
#' @export


load_yoy <- function(dir, file_yoytxt) {
  file.yoytxt <- file.path(dir, file_yoytxt)
  recruits <- read.table(file.yoytxt, header = T)
  return(recruits)
}
