#' Load Atlantis \code{_Biol.prm} file
#'
#' This function loads the Atlantis biology parameter output file.
#' @template dir
#' @parm file_biolprm A character value giving the file name of the biology
#'   \code{.prm" file.
#'   The file should be located in your current working directory or the
#'   folder you specify in \code{dir}.
#'   The argument can also be specified as the full path name, just as long as
#'   argument \code{dir} is specified as \code{NULL}.
#'   Usually the file is named \code{"[...]_Biol.prm".}.
#' @family load functions
#' @return A list of biological parameters from the \code{_Biol.prm} file.
#' @author Kelli Faye Johnson
#' @export

load_biolprm <- function(dir = getwd(), file_biolprm) {
  if (is.null(dir)) {
    file.biolprm <- file_biolprm
  } else {
    file.biolprm <- file.path(dir, file_biolprm)
  }
  biolprm <- read.table(file.biolprm, comment.char = "",
    fill = TRUE, header = FALSE)
  colnames(biolprm) <- seq(dim(biolprm)[2])

  # Find weight-length parameters
  wl <- data.frame(biolprm[grep("li_a_", biolprm[, 1]), 1:2],
    "b" = biolprm[grep("li_b_", biolprm[, 1]), 2])
  wl[, 1] <- gsub("li_a_", "", as.character(wl[, 1]))
  colnames(wl) <- c("group", "a", "b")

  return(list("wl" = wl))
}
