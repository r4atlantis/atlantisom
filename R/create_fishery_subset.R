#' @title Create fishery data from an Altantis scenario
#'
#' @details The function works for a vector of defined species
#'   and only returns information in boxes that are sampled.
#'
#' @author Poseidon
#' @export
#' @template dat
#' @param time    The timing of the survey (a vector indicating specific time steps, which are typically associated with years)
#'                    i.e., seq(365,10*3650,365) would be an annual survey for 10 years
#' @param species The species to sample in the survey (a vector)
#' @param boxes   A vector of box numbers
#' @param effic   Efficiency for each species: a matrix with nrow=length(species). Columns:
#'                 species:    the species name. Matches names in species
#'				   efficiency:
#' @param selex   Selectivity at age. A dataframe defining selectivity at age for each species. Columns are:
#'                 species: the species name. Matches names in species
#'                 agecl:   the age class that selectivity represents
#'                 selex:   the proportion selected relative to fully selected age classes (between 0 and 1)
#'
#' @return A \code{matrix} in the same format as the \code{dat} summed
#'   over layers, with \code{NA} in the layer column.
#'
create_fishery_data <- function(dat, time, species, boxes) {

	#Do some vector length tests (species=effic, column names, )

	#first select the appropriate rows (time and box)
	#first select the appropriate rows and
	aggDat <- aggregateData(dat, time, species, boxes, keepColumns=c("species","agecl","polygon","time"))

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
