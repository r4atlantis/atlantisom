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

  # yoy first row is big number, other rows are smaller numbers
  # convert to same units give absolute # of recruits
  # If 1st time step: divide by (KWSR_RN + KWSR_SN)  # From biol.prm file

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
  # todo: check if all models will be in this time step
  # todo: determine how to match these times with the time
  # step listed in the .nc file
  recruits$Time <- recruits$Time / 365

  # Sum over all boxes/depth/cohorts
  totnums <- aggregate(atoutput ~ species + time, data = nums, sum)
  meanwaa <- aggregate(atoutput ~ species + time, data = test, mean)
  colnames(meanwaa)[colnames(meanwaa) == "atoutput"] <- "meanwaa"

  # Combine recruits and numbers
  # Only pull recruits from the yearly time step, where the
  # yearly time step matches the time step
  totnums <- merge(totnums, recruits,
    by.x = c("time", "species"), by.y = c("Time", "Name"),
    all.x = TRUE, all.y = FALSE)
  totnums <- merge(totnums, meanwaa,
    by = c("species", "time"))
  totnums$recruits[is.na(totnums$recruits)] <- 0

  # Calculate survivors for each species group
  totnums$recruits <- totnums$recruits / totnums$meanwaa / k_wetdry / x_cn
  totnums$survival <- totnums$atoutput - totnums$recruits
  # Make sure time is in order
  totnums <- totnums[order(totnums$species, totnums$time), ]
  # x[n+1] / x[n]
  totnums$survival <- unlist(aggregate(survival ~ species, data = totnums,
        function(x) {
          c(NA, x[-length(x)]) / x})$survival)
  # Use first positive value to replace the initial year and all negative vals
  for(i in seq(NROW(totnums))) {
    totnums$survival[i] <- ifelse(is.na(totnums$survival[i]),
      totnums$survival[i + 1], totnums$survival[i])
    temp <- totnums[i:NROW(totnums), ]
    temp <- temp[temp$species == totnums$species[i], "survival"]
    temp <- temp[temp > 0][1]
    totnums$survival[i] <- ifelse(totnums$survival[i] < 0, temp,
      totnums$survival[i])
  }

  #Calculate Z
  totnums$Z <- -1 * log(totnums$survival)
  finaldata <- data.frame("species" = totnums$species,
    "agecl" = NA, "polygon" = NA, "layer" = NA,
   "time" = totnums$time, "atoutput" = totnums$Z)

  return(finaldata)
}
