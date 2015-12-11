#' Load Atlantis \code{_run.prm} file
#'
#' This function loads the Atlantis biology parameter output file.
#' @template dir
#' @template file_runprm
#' @family load functions
#' @return A list of parameters dictating run characteristics from the
#' \code{_run.prm} file.
#' @author Emma E Hodgson
#' @export
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' load_runprm(d, "VMPA_setas_biol_fishing_Trunk.prm")
#'
load_runprm <- function(dir = getwd(), file_runprm) {
  if (is.null(dir)) {
    file.runprm <- file_runprm
  } else {
    file.runprm <- file.path(dir, file_runprm)
  }
  runprm <- read.table(file.runprm, comment.char = "",
                        fill = TRUE, header = FALSE)
  return(runprm)
}
