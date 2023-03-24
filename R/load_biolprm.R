#' Load Atlantis \code{_Biol.prm} file
#'
#' This function loads the Atlantis biology parameter output file.
#' @template dir
#' @template file_biolprm
#' @family load functions
#' @return A list of biological parameters from the \code{_Biol.prm} file.
#' @author Kelli Faye Johnson
#' @export
#'
#' @examples
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' biolprm <- load_biolprm(d, "Biology.prm")
#' rm(biolprm)
#'
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
  colnames(agespercohort) <- c("group", "agespercohort")

  # Get age of maturity
  # Note that in Atlantis this is the age of ontogenetic diet and habitat shifts
  # Not sexual maturity
  # Renaming in atlantisom output
  ageofmaturity <- biolprm[grep("_age_mat", biolprm[, 1]), 1:2]
  ageofmaturity[, 2] <- as.numeric(as.character(ageofmaturity[, 2]))
  ageofmaturity[, 1] <- gsub("_age_mat", "", ageofmaturity[, 1])
  colnames(ageofmaturity) <- c("group", "ageadultfeedhab")

  # Get maturity ogive, WARNING hardcoded for 10 cohorts
  maturityogive <- biolprm[grep("FSPB_", biolprm[, 1]), 1:2] #name, dimension
  maturityogive[, 2] <- as.numeric(as.character(maturityogive[, 2]))
  maturityogive[, 1] <- gsub("FSPB_", "", maturityogive[, 1])
  maxmat <- max(maturityogive[,2])
  maturityogive[, 3:(maxmat+2)] <- biolprm[grep("FSPB_", biolprm[, 1], value = FALSE)+1, ]
  names(maturityogive) <- c("group", "nagecl",
                            "agecl1", "agecl2", "agecl3", "agecl4", "agecl5",
                            "agecl6", "agecl7", "agecl8", "agecl9"," agecl10")

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
  colnames(kwsr) <- c("group", "kwsr")

  kwrr <- biolprm[grep("KWRR_", biolprm[, 1], ignore.case = TRUE), 1:2]
  kwrr[, 2] <- as.numeric(as.character(kwrr[, 2]))
  kwrr[, 1] <- gsub("KWRR_", "", as.character(kwrr[, 1]))
  colnames(kwrr) <- c("group", "kwrr")

  # Determine which species are vertebrates
  vertebrates <- as.character(biolprm[grep("^flagplankfish", biolprm[, 1]), 1])
  vertebrates <- gsub("flagplankfish", "", vertebrates)

  # Get FSP_XXX proportion of spawner biomass lost due to spawning
  fsp <- biolprm[grep("FSP_", biolprm[, 1]), 1:2]
  fsp[, 2] <- as.numeric(as.character(fsp[, 2]))
  fsp[, 1] <- gsub("FSP_", "", fsp[, 1])
  colnames(fsp) <- c("group", "fsp")

  # note: flagrecruitXXX 3   sets stock recruitment to Beverton Holt,
  # or Beth's forumulation of it
  # eventually check for this and throw an error if not set to 3
  # Get recruitment parameters
  BHalpha <- biolprm[grep("BHalpha_", biolprm[, 1]), 1:2] #name, dimension
  BHalpha[, 2] <- as.numeric(as.character(BHalpha[, 2]))
  BHalpha[, 1] <- gsub("BHalpha_", "", as.character(BHalpha[, 1]))
  colnames(BHalpha) <- c("group", "BHalpha")

  BHbeta <- biolprm[grep("BHbeta_", biolprm[, 1]), 1:2] #name, dimension
  BHbeta[, 2] <- as.numeric(as.character(BHbeta[, 2]))
  BHbeta[, 1] <- gsub("BHbeta_", "", as.character(BHbeta[, 1]))
  colnames(BHbeta) <- c("group", "BHbeta")

  # Optimum ratio of resn to structn (for fecundity, eventually)
  r.rs <- as.numeric(as.character(biolprm[grep("X_RS", biolprm[, 1]), 2]))

  # Find time of spawning
  time_spawn <- biolprm[grep("_Time_Spawn", biolprm[, 1],
    ignore.case = TRUE), 1:2]
  time_spawn[, 2] <- as.numeric(as.character(time_spawn[, 2]))
  time_spawn[, 1] <- gsub("_Time_Spawn", "", as.character(time_spawn[, 1]))
  colnames(time_spawn) <- c("group", "time_spawn")

  # Find length of recruitment period
  recruit_period <- biolprm[grep("Recruit_Period_", biolprm[, 1],
    ignore.case = TRUE), 1:2]
  recruit_period[, 2] <- as.numeric(as.character(recruit_period[, 2]))
  recruit_period[, 1] <- gsub("Recruit_Period_", "",
    as.character(recruit_period[, 1]))
  colnames(recruit_period) <- c("group", "recruit_period")

  # Find length of recruitment period
  recruit_time <- biolprm[grep("_Recruit_Time", biolprm[, 1],
    ignore.case = TRUE), 1:2]
  recruit_time[, 2] <- as.numeric(as.character(recruit_time[, 2]))
  recruit_time[, 1] <- gsub("_Recruit_Time", "",
    as.character(recruit_time[, 1]))
  colnames(recruit_time) <- c("group", "recruit_time")

  # Get predator gape widths
  # lower proportion of prey size accessible by predator
  klp <- biolprm[grep("KLP_", biolprm[, 1],
                      ignore.case = TRUE), 1:2]
  klp[, 2] <- as.numeric(as.character(klp[, 2]))
  klp[, 1] <- gsub("KLP_", "", as.character(klp[, 1]))
  colnames(klp) <- c("group", "klp")

  # upper proportion of prey size accessible by predator
  kup <- biolprm[grep("KUP_", biolprm[, 1],
                      ignore.case = TRUE), 1:2]
  kup[, 2] <- as.numeric(as.character(kup[, 2]))
  kup[, 1] <- gsub("KUP_", "", as.character(kup[, 1]))
  colnames(kup) <- c("group", "kup")

  # Which formulation is used in the model
  hard_feed <- as.numeric(as.character(biolprm[grep("UseHardFeedingWindow", biolprm[, 1]), 2]))
  bilogistic_feed <- as.numeric(as.character(biolprm[grep("UseBiLogisticFeedingWindow", biolprm[, 1]), 2]))
  hump_feed <- as.numeric(as.character(biolprm[grep("UseHumpedFeedingWindow", biolprm[, 1]), 2]))

  kmax <- biolprm[grep("Kmax_coefft_", biolprm[, 1],
                      ignore.case = TRUE), 1:2]
  kmax[, 2] <- as.numeric(as.character(kmax[, 2]))
  kmax[, 1] <- gsub("Kmax_coefft_", "", as.character(kmax[, 1]))
  colnames(kmax) <- c("group", "kmax")


  return(list("wl" = wl, "redfieldcn" = r.cn, "kgw2d" = kgw2d,
    "agespercohort" = agespercohort, "ageofmaturity" = ageofmaturity,
    "maturityogive" = maturityogive, "vertebrates" = vertebrates,
    "kwsr" = kwsr, "kwrr" = kwrr, "fsp" = fsp, "BHalpha" = BHalpha,
    "BHbeta" = BHbeta, "r.rs" = r.rs, "time_spawn" = time_spawn,
    "recruit_period" = recruit_period, "recruit_time" = recruit_time,
    "klp" = klp, "kup" = kup, "hard_feed" = hard_feed,
    "bilogistic_feed" = bilogistic_feed, "hump_feed" = hump_feed,
    "kmax" = kmax))

}
