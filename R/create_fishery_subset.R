#create_fishery_data R function for Atlantis Summitt Poseidon adventure

#' @author Poseidon

#' @param dat 	  The dataframe of numbers-at-age from reading in the Atlantis fileSnapshot
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


#' @details Returns a matrix in same format as the input dat matrix
#' @details columns: species, agecl, polygon, layer, time, atoutput
#' @details  --will sum over layers, but enter NA as layer to indicate all layers
#' @details This function is for a vector of defined species
#' @details Returns only boxes where fishery was sampled

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