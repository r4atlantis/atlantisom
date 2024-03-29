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
#' @importFrom magrittr %>%
#' @template yoy
#' @param nums Object containing the number at stage.
#' @template fgs
#' @template biolprm
#' @param toutinc output time increment from run.prm file
#' @family calc functions
#' @return A \code{data.frame} of time-varying Z values.
#' @author Sean Lucey
#' @export
#'
#' @examples
#' \dontrun{
#' dir <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' file_nc <- "outputs.nc"
#' fgs <- load_fgs(dir = dir, "Functional_groups.csv")
#' file_init <- "Initial_condition.nc"
#' bps <- load_bps(dir = dir, "Functional_groups.csv", file_init = "Initial_condition.nc")
#' select_groups <- fgs$Name[fgs$IsTurnedOn > 0]
#' select_variable <- "Nums"
#' box.info <- load_box(dir = dir, file_bgm="Geography.bgm")
#' bboxes <- get_boundary(box.info)
#' #when calc_stage2age is run in the run_truth, it will need to have the nums
#' #data frame and the bioprm already read in:
#' nums_data <- load_nc(dir = dir,
#'                      file_nc="outputs.nc",
#'                      bps=bps, fgs=fgs, select_groups=select_groups,
#'                      select_variable = "Nums",
#'                      check_acronyms = TRUE, bboxes = bboxes)
#' biolprm <- load_biolprm(dir, file_biolprm="Biology.prm")
#' run <- load_runprm(dir, "Run_settings.xml")
#' yoy <- load_yoy(dir = dir, file_yoy = "outputsYOY.txt")
#' calc_Z(yoy = yoy, nums = nums_data, fgs = fgs, biolprm = biolprm,
#'   toutinc = run$toutinc)
#'}

#calc_Z <- function(yoy, nums, fgs, biolprm, toutinc = 73) {
# harcoding toutinc won't work, now passing from calc_stage2age

calc_Z <- function(yoy, nums, fgs, biolprm, toutinc) {
  # subset the yoy for species included in the fgs file
  # that are turned on
  # colnames of the recruit data are "Time" and a column for each
  # species where the name is the functional group with an additional .0
  turnedon <- fgs[fgs$IsTurnedOn > 0, ]
  recruits <- yoy[, colnames(yoy) %in% c("Time", paste0(turnedon$Code, ".0"))]

  # mg carbon converted to wet weight in tonnes
  k_wetdry <- biolprm$kgw2d / 1000000000
  # Sum of structural and reserve nitrogen (KWSR_RN + KWSR_SN)
  nitro <- merge(biolprm$kwsr, biolprm$kwrr, by = "1")
  nitro$sum <- apply(nitro[, 2:3], 1, sum)

  # legacy: If output is from legacy code there will be an error in the
  # yoy data, where the first row is in a different unit.
  # yoy.txt is the biomass in tonnes per spawning event summed over the total
  # model domain.
  # The first row (< Nov/Dec 2015) is stored as biomass and the remaining rows
  # are stored in numbers, must convert the entire matrix to biomass
  # Check if legacy code and if so convert the numbers to biomass

  # G.Fay 2/21/16 : changed yoy to recruits in below loop.
  if (abs(recruits[1, 2] / recruits[2, 2]) > 10) {
    recruits[2:NROW(recruits), 2:NCOL(recruits)] <- recruits[2:NROW(recruits), 2:NCOL(recruits)] *
    nitro[match(gsub(".0", "", colnames(recruits)[-1]), nitro[, 1]), "sum"]
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
  #recruits$yr <- as.integer(round(recruits$Time)/365) #needed to merge with totnums

  # June 2019:Isaac determined that yr 0 (Time 0) in the YOY file is not real, don't use it
  # code below assigns year 1 to times 0:stepperyr, all good output timesteps in truth$nums


  # G.Fay 2/21/16
  # UGLY code below tries to align fraction of annual yoy with timing of recruitment
  # values in YOY.txt are total YOY that year waiting to recruit.
  # seems to get rid of most of 'issues' - some v.minor survival >1,
  #perhaps due to averaging of survival over toutinc days
  nyrs <- ceiling(max(yoy$Time)/365)
  times <- unique(yoy$Time)

  # SKG June 2019
  # need to match codes below, this mismatches when biolprm not in same order!!
  #recstart_temp <- biolprm$recruit_time
  #recstart_temp[,2] <- biolprm$time_spawn[-(grep('#',
  #                     biolprm$time_spawn[,1])),2] + biolprm$recruit_time[,2]
  # was this grep to get rid of a species with #XXX? breaks when there are none
  #recstart_temp[,2] <- biolprm$time_spawn[,2] + biolprm$recruit_time[,2]

  #recstart_temp <- recstart_temp[recstart_temp[,1]%in%turnedon$Code,] #bug added codes with no YOY output
  #recstart_temp <- recstart_temp[recstart_temp[,1]%in%recruits$group,]

  # June 12 2019 Beth determined that we do not need spawn_period

  # rectiming is a dataframe with species code, time_spawn (day of year),
  # recruit_time (number of days), and recruit period (number of days).
  # we use these to calculate recstart (day of year) and recend (day of year)
  rectiming <- merge(biolprm$time_spawn, biolprm$recruit_time, by = 1)
  rectiming <- merge(rectiming, biolprm$recruit_period, by = 1)
  names(rectiming) <- c("Code", "time_spawn", "recruit_time", "recruit_period")
  rectiming <- rectiming %>%
    dplyr::mutate(recstart = time_spawn + recruit_time) %>%
    dplyr::mutate(recend = recstart + recruit_period)

  # Sum numbers output over all boxes/depth/cohorts
  # align model output timesteps (days) with recruitment periods (days)
  totnums <- aggregate(atoutput ~ species + time, data = nums, sum) %>%
    #mutate(time.days = (time+1)*toutinc) %>% #makes time 0 into days 0->73, etc
    dplyr::mutate(time.days = (time)*toutinc)   %>% #makes time 1 into days 0->73, etc
    dplyr::mutate(yr = ceiling(time.days/365))  # yr 1 is 0:stepsperyr to match recruits yr1

  totnums <- merge(totnums, recruits,
                   by.x = c("time.days", "species"), by.y = c("Time", "Name"),
                   all.x = TRUE) %>%
    dplyr::arrange(time)

  #subset recpars to groups of interest
  recstart_temp <- rectiming[rectiming$Code %in% totnums$group,]

  totnums$frac_recruit <- 0

  for (irow in 1:nrow(recstart_temp)) {
    group <- recstart_temp$Code[irow]
    pick <- which(totnums$group == group)

    recstart <- seq(recstart_temp$recstart[irow],by=365,length.out=nyrs)
    recstart <- recstart[recstart<max(totnums$time.days[pick])]
    recend <- recstart + recstart_temp$recruit_period[irow]
    #rec_times <- rbind(rec_times,cbind(group,recstart,recend))

    for (i_rec in 1:length(recstart)) {
      i_tstart <- which(pick==min(pick[totnums$time.days[pick]>=recstart[i_rec]]))
      i_tstop <- which(pick==min(pick[totnums$time.days[pick]>=recend[i_rec]]))
      if (i_rec == length(recstart)) i_tstop <- length(pick)
      if(length(i_tstart) == 0) break;
      n_t <- 1+i_tstop-i_tstart
      for (i_t in 1:n_t) {
        t_temp <- totnums$time.days[pick[i_tstart+i_t-1]]
        num_temp <- t_temp - recstart[i_rec]
        if (i_t>1) {
          if ((recend[i_rec]-t_temp)>toutinc) {
            num_temp <- toutinc
          }
          else {
            num_temp <- recend[i_rec]-(totnums$time.days[pick[i_tstart+i_t-2]])
          }
        }
        frac_temp <- max(c(0,num_temp /(recend[i_rec]-recstart[i_rec])))
        if(frac_temp > 1.0) frac_temp = 1.0
        totnums$frac_recruit[pick[i_tstart+i_t-1]] <- frac_temp
      }
    }
  }

  #G.Fay 1/6/16, expand to num of recruits
  # Recruit / mg C converted to wet weight in tonnes / redfield ratio of C:N
  totnums$recruits <- totnums$recruits/k_wetdry/biolprm$redfieldcn

  totnums$group <- recruits$group[match(totnums$species, recruits$Name)]
  # For all time increments where there
  totnums$recruits[is.na(totnums$recruits)] <- 0

  totnums$annrecruits <- totnums$recruits

  totnums$recruits <- totnums$annrecruits*totnums$frac_recruit

  # Calculate survivors for each species group
  totnums$survivors <- totnums$recruits

  totnums$survival <- totnums$survivors
  # Calculate survival for each group
  for (group in unique(totnums$group)) {
    #if(group == "SHD") browser()
    pick <- which(totnums$group == group)
    survival_temp <- c(NA,
      totnums$survivors[pick[-1]]/totnums$atoutput[pick[-length(pick)]])

    # G.Fay 2/21/16  "think" this is what things should be, recruits don't show up
    # in numbers at age until time step after the recruitment event.
     survival_temp <- c(
       (totnums$atoutput[pick[-1]]-
          totnums$recruits[pick[-1]])/totnums$atoutput[pick[-length(pick)]],NA)

#     survival_temp <- c(
#       (totnums$atoutput[pick[-1]])/
#          (totnums$recruits[pick[-1]]+totnums$atoutput[pick[-length(pick)]]),NA)

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
