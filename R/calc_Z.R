#' Calculate total mortality for age structured groups
#'
#' @description Calculate total mortality for all age-structured groups
#'   in an Atlantis model. The mortality values are then used in other
#'   functions to translate stages to ages: \code{\link{calc_stage2age}}.
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
#' @template toutinc
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
#' #when calc_stage2age is run in the run_truth, it will need to have the nums
#' #data frame and the bioprm already read in:
#' nums_data <- load_nc(dir = dir,
#'                      file_nc="outputSETAS.nc",
#'                      bps=bps, fgs=fgs, select_groups=select_groups,
#'                      select_variable = "Nums",
#'                      check_acronyms = TRUE, bboxes = bboxes)
#' biolprm <- load_biolprm(dir, file_biolprm="VMPA_setas_biol_fishing_Trunk.prm")
#' run <- load_runprm(dir, "VMPA_setas_run_fishing_F_Trunk.xml")
#' yoy <- load_yoy(dir = dir, file_yoy = "outputSETASYOY.txt")
#' calc_Z(yoy = yoy, nums = nums_data, fgs = fgs, biolprm = biolprm,
#'   toutinc = run$toutinc)
#'
calc_Z <- function(yoy, nums, fgs, biolprm, toutinc = 73) {

  # subset the yoy for species included in the fgs file
  # that are turned on
  # colnames of the recruit data are "Time" and a column for each
  # species where the name is the functional group with an additional .0
  turnedon <- fgs[fgs$IsTurnedOn > 0, ]
  recruits <- yoy[, colnames(yoy) %in% c("Time", paste0(turnedon$Code, ".0"))]

  # mg carbon converted to wet weight in tonnes
  k_wetdry <- biolprm$kgw2d / 1000000000
  # Sum of structural and reserve nitrogen (KWSR_RN + KWSR_SN)
  nitro <- merge(biolprm$kswr, biolprm$kwrr, by = "1")
  nitro$sum <- apply(nitro[, 2:3], 1, sum)

  # legacy: If output is from legacy code there will be an error in the
  # yoy data, where the first row is in a different unit.
  # yoy.txt is the biomass in tonnes per spawning event summed over the total
  # model domain.
  # The first row (< Nov/Dec 2015) is stored as biomass and the remaining rows
  # are stored in numbers, must convert the entire matrix to biomass
  # Check if legacy code and if so convert the numbers to biomass
  if (abs(yoy[1, 2] / yoy[2, 2]) > 10) {
    yoy[2:NROW(yoy), 2:NCOL(yoy)] <- yoy[2:NROW(yoy), 2:NCOL(yoy)] *
    nitro[match(gsub(".0", "", colnames(yoy)[-1]), nitro[, 1]), "sum"]
  }

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

  # merge recruits with strucn and resn of recruits from biol.prm file
  recruits <- merge(recruits, nitro[, c(1, 4)],
    by.x = "group", by.y = "1", all.x = TRUE, all.y = FALSE)
  colnames(recruits)[which(colnames(recruits) == "recruits")] <- "recruitsbio"
  # Get recruits in numbers rather than biomass
  recruits$recruits <- recruits$recruitsbio / recruits$sum

  # match "Time" of the young of the year with the time-step periodicity
  # listed in the run.prm or run.xml file
  recruits$Time <- recruits$Time / toutinc

  #G.Fay 1/6/16, expand to num of recruits
  # Recruit / mg C converted to wet weight in tonnes / redfield ratio of C:N
  recruits$recruits <- recruits$recruits/k_wetdry/biolprm$redfieldcn

  # Sum over all boxes/depth/cohorts
  totnums <- aggregate(atoutput ~ species + time, data = nums, sum)

  # Combine recruits and numbers
  # Only pull recruits from the yearly time step, where the
  # yearly time step matches the time step
  totnums <- merge(totnums, recruits,
    by.x = c("time", "species"), by.y = c("Time", "Name"),
    all.x = TRUE, all.y = FALSE)
  totnums$group <- recruits$group[match(totnums$species, recruits$Name)]
  # For all time increments where there
  totnums$recruits[is.na(totnums$recruits)] <- 0

  # Calculate survivors for each species group
  totnums$survivors <- totnums$atoutput - totnums$recruits
  # Make sure time is in order
  totnums <- totnums[order(totnums$species, totnums$time), ]

  totnums$survival <- totnums$survivors
  # Calculate survival for each group
  for (group in unique(totnums$group)) {
    if(group == "SHD") browser()
    pick <- which(totnums$group == group)
    survival_temp <- c(NA,
      totnums$survivors[pick[-1]]/totnums$atoutput[pick[-length(pick)]])
    survival_temp[survival_temp < 0] <- NA
    # Use first positive value to replace the initial year and all negative vals
    firstgood <- which(!is.na(survival_temp))[1]

    survival_temp[1:firstgood] <- survival_temp[firstgood]
    for(ii in seq_along(survival_temp)) {
      if (is.na(survival_temp[ii])) {
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
