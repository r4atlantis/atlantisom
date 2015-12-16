#' Load Atlantis \code{_Biol.prm} file
#'
#' This function loads the Atlantis biology parameter output file.
#' @template dir
#' @template file_biolprm
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
  r.cn <- as.numeric(as.character(biolprm[grep("X_CN", biolprm[, 1]), 2]))

  # Get kilogram wet to dry
  kgw2d <- as.numeric(as.character(biolprm[grep("k_wetdry", biolprm[, 1]), 2]))

  # Get ages per cohort
  agespercohort <- biolprm[grep("_AgeClassSize", biolprm[, 1]), 1:2]
  agespercohort[, 2] <- as.numeric(as.character(agespercohort[, 2]))
  agespercohort[, 1] <- gsub("_AgeClassSize", "", agespercohort[, 1])

  # Get age of maturity
  ageofmaturity <- biolprm[grep("_age_mat", biolprm[, 1]), 1:2]
  ageofmaturity[, 2] <- as.numeric(as.character(ageofmaturity[, 2]))
  ageofmaturity[, 1] <- gsub("_age_mat", "", ageofmaturity[, 1])

  # Find weight-length parameters
  wl <- data.frame(biolprm[grep("li_a_", biolprm[, 1]), 1:2],
    "b" = biolprm[grep("li_b_", biolprm[, 1]), 2])
  wl[,2] <- as.numeric(as.character(wl[,2]))
  wl[,3] <- as.numeric(as.character(wl[,3]))
  wl[, 1] <- gsub("li_a_", "", as.character(wl[, 1]))
  colnames(wl) <- c("group", "a", "b")

  # Find structural and reserve nitrogen of recruits
  kwsr <- biolprm[grep("KWSR_", biolprm[, 1], ignore.case = TRUE), 1:2]
  kwsr[, 2] <- as.numeric(as.character(kwsr[, 2]))
  kwsr[, 1] <- gsub("KWSR_", "", as.character(kwsr[, 1]))

  kwrr <- biolprm[grep("KWRR_", biolprm[, 1], ignore.case = TRUE), 1:2]
  kwrr[, 2] <- as.numeric(as.character(kwrr[, 2]))
  kwrr[, 1] <- gsub("KWRR_", "", as.character(kwrr[, 1]))

  # Determine which species are vertebrates
  vertebrates <- as.character(biolprm[grep("^flagplankfish", biolprm[, 1]), 1])
  vertebrates <- gsub("flagplankfish", "", vertebrates)

  return(list("wl" = wl, "redfieldcn" = r.cn, "kgw2d" = kgw2d,
    "agespercohort" = agespercohort, "vertebrates" = vertebrates,
    "kswr" = kwsr, "kwrr" = kwrr))
}
