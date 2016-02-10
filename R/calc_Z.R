#' Calculate total mortality for age structured groups
#'
#' @description
#'
#' @details Steps included when translating from stages (i.e.,
#'   multiple age classes in a single group) to individual age
#'   classes are:
#'   \itemize{
#'     \item obtain numbers at age from each time-step
#'     \item obtain numbers of recruits for each time-step, where the
#'       information provided in the \code{[...]YOY.txt} or \code{yoy}
#'       object is provided in biomass and must be converted to numbers.
#'     \item calculate total mortality for each time step, where
#'       \deqn{survival_{t} = (\sum(nastage_{t}) - recruits_{t}) / \sum{nastage_{t-1}}}
#'       \deqn{Z_{t} = -1 * ln(survival_{t})}
#'
#'   }
#' This function uses the YOY.txt and Nums to calculate Z.
#' @template yoy
#' @param nums Object containing the number at stage.
#' @template fgs
#' @template biolprm
#' @family calc functions
#' @return A \code{data.frame} of time-varying Z values.
#' @author Sean Lucey
#' @export
#'
#' @examples
#' dir <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file_nc <- "outputSETAS.nc"
#' fgs <- load_fgs(dir = dir, "functionalGroups.csv")
#' file_init <- "INIT_VMPA_Jan2015.nc"
#' bps <- load_bps(dir = dir, fgs, file_init)
#' select_groups <- fgs$Name[fgs$IsTurnedOn > 0]
#' select_variable <- "Nums"
#' box.info <- load_box(dir = dir, file_bgm="VMPA_setas.bgm")
#' bboxes <- get_boundary(box.info)
#' #when calc_stage2age is run in the run_atlantis, it will need to have the nums
#' #data frame and the bioprm already read in:
#' nums_data <- load_nc(dir = dir,
#'                      file_nc="outputSETAS.nc",
#'                      bps=bps, fgs=fgs, select_groups=select_groups,
#'                      select_variable = "Nums",
#'                      check_acronyms = TRUE, bboxes = bboxes)
#' biolprm <- load_biolprm(dir, file_biolprm="VMPA_setas_biol_fishing_Trunk.prm")
#' yoy <- load_yoy(dir = dir, file_yoy = "outputSETASYOY.txt")
#' calc_Z(yoy = yoy, nums = nums_data, fgs = fgs, biolprm = biolprm)
calc_Z <- function(yoy, nums, fgs, biolprm) {

  # subset the yoy for those species that are included in the fgs file
  # that are turned on
  species.code <- fgs$Code
  turnedon <- fgs[fgs$IsTurnedOn > 0, ]
  names <- paste0(turnedon$Code, ".0")
  recruits <- yoy[, colnames(yoy) %in% c("Time", names)]

  # Redfield ratio of carbon to nitrogen
  x_cn <- as.numeric(as.character(biolprm$redfieldcn))
  # mg carbon converted to wet weight in tonnes
  k_wetdry <- as.numeric(as.character(biolprm$kgw2d))
  k_wetdry <- k_wetdry / 1000000000

  # Wide to long
  recruits <- reshape(data = recruits, direction = "long",
    varying = colnames(recruits)[-1],
    v.names = "recruits",
    times = colnames(recruits)[-1],
    timevar = "group")
  rownames(recruits) <- 1:NROW(recruits)
  recruits <- recruits[, -which(colnames(recruits) == "id")]
  # Switch from species code to species
  recruits$group <- gsub("\\.0", "", recruits$group)
  recruits <- merge(recruits, fgs[, c("Code", "Name")],
    by.x = "group", by.y = "Code")

  # yoy first row is big number, other rows are smaller numbers
  # convert to same units give absolute # of recruits
  # If 1st time step: divide by (KWSR_RN + KWSR_SN)  # From biol.prm file
  # merge recruits with strucn and resn of recruits from biol.prm file
  # todo: add more notes here especially those from the Atlantis wiki
  nitro <- merge(biolprm$kswr, biolprm$kwrr, by = "1")
  nitro$sum <- apply(nitro[, 2:3], 1, sum)
  recruits <- merge(recruits, nitro[, c(1, 4)],
    by.x = "group", by.y = "1", all.x = TRUE, all.y = FALSE)
  colnames(recruits)[which(colnames(recruits) == "recruits")] <- "recruitsbio"
  recruits$recruits <- recruits$recruitsbio / recruits$sum

  # todo: check if all models will be in this time step
  # todo: determine how to match these times with the time
  # step listed in the .nc file
  recruits$Time <- recruits$Time / 365


  #G.Fay 1/6/16, expand to num of recruits
  recruits$recruits <- recruits$recruits/k_wetdry/x_cn

  # Sum over all boxes/depth/cohorts
  totnums <- aggregate(atoutput ~ species + time, data = nums, sum)

  # Combine recruits and numbers
  # Only pull recruits from the yearly time step, where the
  # yearly time step matches the time step
  totnums <- merge(totnums, recruits,
    by.x = c("time", "species"), by.y = c("Time", "Name"),
    all.x = TRUE, all.y = FALSE)
  totnums$recruits[is.na(totnums$recruits)] <- 0

  # Calculate survivors for each species group
  totnums$survivors <- totnums$atoutput - totnums$recruits
  # Make sure time is in order
  totnums <- totnums[order(totnums$species, totnums$time), ]

  totnums$survival <- totnums$survivors
  # Calculate survival for each group
  for (group in unique(totnums$group)) {
    if (is.na(group)) next
    pick <- which(totnums$group == group)
    survival_temp <- c(NA,
      totnums$survivors[pick[-1]]/totnums$atoutput[pick[-length(pick)]])
    survival_temp[survival_temp < 0] <- NA
    # Use first positive value to replace the initial year and all negative vals
    firstgood <- which(!is.na(survival_temp))[1]
    survival_temp[1:firstgood] <- survival_temp[firstgood]
    for(ii in seq_along(survival_temp)) {
      if (survival_temp[ii] < 0) {
        nonzero <- which(which(survival_temp > 0) > ii)
        if (length(nonzero) == 0) nonzero <- which(survival_temp > 0)
        survival_temp[ii] <- survival_temp[which.min(abs(nonzero - ii))]
      }
    }
    totnums$survival[pick] <- survival_temp
   }

  #Calculate Z
  totnums$Z <- -1 * log(totnums$survival)
  finaldata <- data.frame("species" = totnums$species,
    "agecl" = NA, "polygon" = NA, "layer" = NA,
   "time" = totnums$time, "atoutput" = totnums$Z)

  return(finaldata)
}
