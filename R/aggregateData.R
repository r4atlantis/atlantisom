#aggregate and select data for Atlantis Summitt Poseidon adventure

#' @author Poseidon

#' @param dat 	  The dataframe of numbers-at-age from reading in the Atlantis fileSnapshot
#' @param time    The timing of the survey (can be a vector if survey spans multiple time periods)
#' @param species The species to sample in the survey (a vector)
#' @param boxes   A vector of box numbers

#' @details Returns a subsetted matrix with columns not aggregated over
#' @details columns: species, agecl, polygon, layer, time, atoutput
#' @details  --will sum over layers, but enter NA as layer to indicate all layers
#' @details This function is for a vector of defined species 


aggregateData <- function(dat, time, species, boxes, keepColumns=c("species","agecl","polygon", "time")) {

	#Do some vector length tests (species=effic, column names, )

	#first select the appropriate rows (time and box)
	sampDat <- dat[dat$time%in%time & dat$polygon%in%boxes & dat$species%in%species, ]

	#sum over a variable
	aggDat <- aggregate(sampDat$atoutput, as.list(sampDat[,c(keepColumns)]), sum)
	names(aggDat) <- c(keepColumns,"numAtAge")

	return(aggDat)
}



#example
if(F) {

	dat <- data.frame(species = c(rep("spec1",3*3),rep("spec2",5*3)),
		              agecl = c(rep(1:3,3),rep(3:7,3)), 
		              polygon = c(rep(1:3,each=3),rep(1:3,each=5)), 
		              layer = 1, 
		              time = 1)

    dat$atoutput <- 10000/dat$agecl

    boxes <- 1:2


	tmp <- aggregateData(dat=dat, time=1, species=c("spec1","spec2"), boxes=boxes)
}