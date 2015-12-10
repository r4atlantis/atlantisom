#create_survey R function for Atlantis Summitt Poseidon adventure

#' @author Poseidon

#' @param dat 	  The dataframe of numbers-at-age from reading in the Atlantis files
#' @param time    The timing of the survey (a vector indicating specific time steps, which are typically associated with years)
#'                    i.e., seq(365,10*3650,365) would be an annual survey for 10 years
#' @param species The species to sample in the survey (a vector)
#' @param spex    The specifications of Atlantis model (box-specific area, habitat, etc.)
#'                   for now, from load_boxarea a dataframe with polygon and area column names
#' @param boxes A matrix with two columns: 
#'		1) polygon:  box ID's that are sampled
#'		2) survArea: area sampled in that box
#' @param effic Efficiency for each species: a matrix with nrow=length(species). Columns:
#'                 species:    the species name. Matches names in species
#'				   efficiency: 
#' @param selex Selectivity at age. A dataframe defining selectivity at age for each species. Columns are:
#'                 species: the species name. Matches names in species
#'                 agecl:   the age class that selectivity represents
#'                 selex:   the proportion selected relative to fully selected age classes (between 0 and 1)


#' @details Returns a matrix similar to the input matrix
#' @details columns: species, agecl, polygon, layer, time, atoutput
#' @details  --will sum over layers, but enter NA as layer to indicate all layers
#' @details This function is for a vector of defined species 


create_survey <- function(dat, time, species, spex, boxes, effic, selex) {

	#Do some vector length tests (species=effic, column names, )

	#first select the appropriate rows and 
	aggDat <- aggregateData(dat, time, species, boxes$polygon, keepColumns=c("species","agecl","polygon","time"))

NNED TO RETHINK TIME


	#now calculate density in each box from num-at-age and total area by habitat
	dens <- merge(aggDat,spex[,c("polygon","area")],by="polygon",all.x=T)
	dens$density <- dens$numAtAge / dens$area

	#Habitat? Atlantis is already modifying density? We assume that survey is acting homogenously across all habitats

	#Calculate numbers-at-age surveyed using area surveyed, efficiency, and selex-at-age
	##merge in surveyed area
	surv <- merge(dens,boxes,by="polygon",all.x=T)
	#merge in efficiency
	surv <- merge(surv,effic,by="species",all.x=T)
	#merge in selex
	surv <- merge(surv,selex,by=c("species","agecl"),all.x=T)
	#should I change any missing selex to zero???
	#should I scale or check selex maximum at 1?

	surv$numAtAgeSurv <- surv$density * surv$survArea * surv$efficiency * surv$selex

	#Should I be checking for NA's along the way to identify problems?


	#Create final dataframe in same format as input
	#put time (mean) and layers (NA) back in the dataframe for completeness
	out <- data.frame(species = surv$species, 
		              agecl = surv$agecl, 
		              polygon = surv$polygon, 
		              layer = NA, 
		              time = time,
		              atoutput = surv$numAtAgeSurv)

	return(out)


}


#example
if(F) {

	dat <- data.frame(species = c(rep("spec1",3*3),rep("spec2",5*3)),
		              agecl = c(rep(1:3,3),rep(3:7,3)), 
		              polygon = c(rep(1:3,each=3),rep(1:3,each=5)), 
		              layer = 1, 
		              time = 1)

    dat$atoutput <- 10000/dat$agecl

    spex <- data.frame(polygon=1:3,area=c(1000,2000,3000))

    boxes <- data.frame(polygon=1:2,survArea=c(10,200))

	effic <- data.frame(species=c("spec1","spec2"), efficiency=c(0.1,0.5))

	selex <- data.frame(species=c(rep("spec1",3),rep("spec2",7)), 
		                agecl=c(1:3,1:7),
		                selex=c(0.1,0.5,1,0,0.1,0.3,0.5,0.7,1,1))



	tmp <- create_survey(dat=dat, time=1, species=c("spec1","spec2"), spex=spex, boxes=boxes, effic=effic, selex=selex)
}