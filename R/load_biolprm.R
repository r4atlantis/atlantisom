#' Load Atlantis \code{_Biol.prm} file
#'
#' This function loads the Atlantis biology parameter output file.
#' @template dir
#' @parm file_biolprm A character value giving the file name of the biology
#'   \code{.prm} file.
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

  # Get Redfield CN ratio
  #browser()
  r.cn <- biolprm[grep("X_CN", biolprm[, 1]), 2]

  # Get kilogram wet to dry
  kgw2d <- biolprm[grep("k_wetdry", biolprm[, 1]), 2]

  # Get ages per cohort
  agespercohort <- biolprm[grep("_AgeClassSize", biolprm[, 1]), 1:2]
  agespercohort[, 1] <- gsub("_AgeClassSize", "", agespercohort[, 1])

  # Get age of maturity
  ageofmaturity <- biolprm[grep("_age_mat", biolprm[, 1]), 1:2]
  ageofmaturity[, 1] <- gsub("_age_mat", "", ageofmaturity[, 1])

  # Find weight-length parameters
  wl <- data.frame(biolprm[grep("li_a_", biolprm[, 1]), 1:2],
    "b" = biolprm[grep("li_b_", biolprm[, 1]), 2])
  wl[, 1] <- gsub("li_a_", "", as.character(wl[, 1]))
  colnames(wl) <- c("group", "a", "b")

  return(list("wl" = wl, "redfieldcn" = r.cn, "kgw2d" = kgw2d,
    "agespercohort" = agespercohort))
}

# load_all()
# load_biolprm("data", "CalCurrentV3_Biol.prm")
