#' Load Atlantis \code{_run.prm} file
#'
#' This function loads the Atlantis biology parameter output file.
#' @template dir
#' @param file_runprm A character value giving the file name of the biology
#'   \code{.prm} file.
#'   The file should be located in your current working directory or the
#'   folder you specify in \code{dir}.
#'   The argument can also be specified as the full path name, just as long as
#'   argument \code{dir} is specified as \code{NULL}.
#'   Usually the file is named \code{"[...]_run.prm".}.
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
