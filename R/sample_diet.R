#' @title Sample total consumption to create diet composition data
#'
#' @description Create sampled diet composition data from the total consumption
#'    in an Atlantis scenario. Observation error and bias are added.
#'
#' @details The function takes total consumption data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{???}. One does
#'   not need to use these functions to create \code{dat}, rather you must only
#'   ensure that the structure of \code{dat} is the same.
#'   Currently, the function creates sampled diet composition by removing
#'   non-sampled and non-enumerated species groups from the total
#'   consumption table from Atlantis.
#'   Bias is added by setting infrequently consumed (<0.25) prey groups to
#'   zero at random.
#'   Error is incorporated into proportional composition entries by
#'   adding uniform error to true values to half of the observations.
#'   The function adjusts the remaining table so each row sums to one
#'   before returning the "observed" mean diet composition summary table.
#'   The function needs to be generalized to any Atlantis system by
#'   selecting common species group identifiers to remove from the table.
#'   Also, more realistic observation error and bias distributions
#'   could be applied to obtain realistic diet composition data.

#' @author Robert Wildermuth
#' @export
#' @param dat A \code{data.frame} containing sampled predator species
#'   identifiers  in the first
#'   column and prey species consumption proportions in the remaining columns.
#' @examples
#' 		setwd(file.path(system.file( package = "atlantisom"),".."))
#'
#'		directory <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#'		scenario <- "SETAS"
#'		groups <- load_fgs(dir = directory, "functionalGroups.csv")
#'		groups <- groups[groups$IsTurnedOn > 0, "Name"]
#'		results <- run_atlantis(scenario = scenario,
#'		dir = directory,
#'		file_fgs = "functionalGroups.csv",
#'		file_bgm = "VMPA_setas.bgm",
#'		select_groups = groups,
#'		file_init = "INIT_VMPA_Jan2015.nc",
#'		file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")
#'
#'		# rows should each sum to one:
#'		rowSums(dat[,2:ncol(dat)])
#'		dim(dat)
#'
#'		obsDietComp <- sample_diet(dat)
#'		dim(obsDietComp)
#'

sample_diet <- function(dat) {

  # first remove species not sampled and not quantified in gut analyses
  # !!!RW: Will need some way to generalize this to the diff ecosystems
  nonSampled <- c("SB", "PIN", "REP", "RWH", "BWH", "SWH", "TWH", "INV", "LSQ",
                  "ISQ", "SCA", "QHG", "CLA", "BFF", "BG", "LOB", "RCB", "BMS",
                  "NPW", "OPW", "ZL", "BD", "MA", "MB", "SG", "BC", "ZG", "PL",
                  "DF", "PS", "ZM", "ZS", "PB", "BB", "BO", "DL", "DR", "DC")

  notEnum <- c("SB", "PIN", "REP", "RWH", "BWH", "SWH", "TWH", "INV", "LSQ", "ISQ", "BFF", "BG",
               "ZL", "BD", "MA", "MB", "SG", "BC", "ZG", "PL", "DF", "PS", "ZM", "ZS", "PB",
               "BB", "BO", "DL", "DR")

  dat <- dat[!(dat$Predator %in% nonSampled), !(colnames(dat) %in% notEnum)]

  # add uniform error to half of the "observations"
  nPreyObs <- nrow(dat) * (ncol(dat)-1)
  for(obs in 1:(nPreyObs/2)){
    # determine row and column indices
    rowR <- sample(1:nrow(dat), 1)
    colC <- sample(2:ncol(dat), 1)

    dat[rowR, colC] <- dat[rowR, colC] + runif(1, -0.1, 0.1)
  }

  # add bias by removing little-observed prey at random
  for(i in 1:nrow(dat)){
    for(j in 2:ncol(dat)){
      if(dat[i,j] < 0.25 & runif(1) < 0.15){
        dat[i,j] <- 0
      }
    }
  }

  # recalibrate so that rows add to 1
  # first need to adjust/account for negative values
  baseAdd <- min(dat[,2:ncol(dat)])
  if(baseAdd < 0){

    for(i in 1:nrow(dat)){
      for(j in 2:ncol(dat)){
        if(dat[i,j] != 0){
          dat[i,j] <- dat[i,j]-baseAdd
        }
      }
    }

  }


  for(r in 1:nrow(dat)){

    denom <- rowSums(dat[r,2:ncol(dat)])
    dat[r,2:ncol(dat)] <- (dat[r,2:ncol(dat)])/denom
  }

  return(dat)
}


