#' @title Create fishery subset from Atlantis output

#' @description Create a subset of the fishery observations for an Atlantis scenario.

#' @details The function takes fishery catch data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{\link{load_nc}}
#'   within \code{\link{run_truth}}. One does not need to use these functions
#'   to create \code{dat}, rather you must only ensure that the structure of
#'   \code{dat} is the same.
#'   Currently, the function subsets the data by polygon and time (there are not layers
#'   in fishery outputs).  The input is fishery observations (either catch nums
#'   or catch bio outputs of run_truth), so nothing else needs to be done. Atlantis
#'   already applies a fishery efficiency and selectivity internally.
#'   This function works for specific defined species, specific defined polygons,
#'   and specific defined time.

#' @author Poseidon
#' @export

#' @template      dat
#' @param time    The timing of the survey (a vector indicating specific time steps, which are typically associated with years)
#'                    i.e., seq(365,10*3650,365) would be an annual survey for 10 years
#' @param species The species to sample in the survey (a vector)
#' @param boxes   A vector of box numbers

#' @return The function returns a subsetted matrix with the same columns
#'   as the input data, i.e.,:
#'   \itemize{
#'     \item{species}
#'     \item{agecl}
#'     \item{polygon}
#'     \item{layer}
#'     \item{time}
#'     \item{atoutput}
#'   }
#' This function is for a vector of defined species
#' Returns only boxes where fishery was sampled

create_fishery_subset <- function(dat, time, species, boxes) {

	#Do some vector length tests (species=effic, column names, )

	#first select the appropriate rows (time and box)
	#first select the appropriate rows and
	aggData <- aggregateData(dat, time, species, boxes, keepColumns=c("species","agecl","polygon","time"))

	#Should I be checking for NA's along the way to identify problems?

	#Create final dataframe in same format as input
	#put time (mean) and layers (NA) back in the dataframe for completeness
	out <- data.frame(species = aggData$species,
		              agecl = aggData$agecl,
		              polygon = aggData$polygon,
		              layer = NA,
		              time = aggData$time,
		              atoutput = aggData$numAtAge)

	return(out)
}
