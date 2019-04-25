#' @title Sample a biomass index of abundance from an atlantis scenario
#'
#' @description The function takes numbers-at-age data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{\link{load_nc}}
#'   within \code{\link{run_truth}}. One does not need to use these functions
#'   to create \code{dat}, rather you must only ensure that the structure of
#'   \code{dat} is the same.
#' @details
#'   This function simply calculates biomass-at-age by applying a weight-at-age
#'   vector, sums over polygons,
#'   and then applies user defined error to the biomass.
#'   The result is a coastwide biomass estimate from the survey
#'   Improvements could be to provide polygon specific biomass,
#'   but the cv will need to be thought about.
#' @author Poseidon
#' @export
#'
#' @template dat
#' @param cv      Coefficient of variation for the entire species specific biomass
#'                    a matrix with columns: species, cv
#' @param wtAtAge Weight-at-age by species. a matrix with columns:
#'                   species, agecl, wtAtAge
#'
#' @return The standard dataframe as specified used in \code{dat}.
#'   The function sums over layers and makes \code{$layers} is {NA}.
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' groups <- load_fgs(dir = d, "functionalGroups.csv")
#' run_truth(scenario = "SETAS",
#'   dir = d,
#'   file_fgs = "functionalGroups.csv",
#'   file_bgm = "VMPA_setas.bgm",
#'   select_groups = groups[groups$IsTurnedOn > 0, "Name"],
#'   file_init = "INIT_VMPA_Jan2015.nc",
#'   file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm",
#'   file_runprm = "VMPA_setas_run_fishing_F_Trunk.xml")
#'
#'	species=c("Pisciv_T_Fish","Pisciv_S_Fish")
#'  boxes <- 1:3
#'	effic <- data.frame(species=c("Pisciv_T_Fish","Pisciv_S_Fish"), efficiency=c(0.3,0.1))
#'	selex <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_S_Fish",10)),
#'                agecl=c(1:10,1:10),
#'	                selex=c(0,0,0.1,0.5,0.8,1,1,1,1,1,0,0,0.1,0.3,0.5,0.7,0.9,1,1,#'1))
#'
#'	tmp <- create_survey(dat=results$nums, time=seq(70,82,3), species=species, boxes=boxes, effic=effic, selex=sel#'ex)
#'
#'	wtAtAge <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_S_Fish",10)),
#'	                agecl=c(1:10,1:10),
#'	                wtAtAge=c(0.1,0.5,0.9,1.5,1.8,2.0,2.1,2.2,2.3,2.35,0.05,0.2,0.3,0.35,0.45,0.5,0.55,0.6,0.63,0.65))
#'	cv <- data.frame(species=species, cv=c(0.2,0.3))
#'
#'	survObsBiom <- sample_survey_biomass(dat=tmp,cv=cv,wtAtAge)



sample_survey_biomass <- function(dat,cv,wtAtAge) {

	#calculate total biomass estimate and partition to boxes
	###  otherwise some assumptions about box-specific cv have to be made
	###  this makes sure that box-specific biomasses add up to total observed biomass
    ### use create_survey to subset the boxes and time

	#convert numAtAge to BiomassAtAge
	dat2 <- merge(dat,wtAtAge,by=c("species","agecl"),all.x=T)
	dat2$biomass <- dat2$atoutput * dat2$wtAtAge

	#sum over boxes and ages (the sampled boxes were already subset in create functions)
	totB <- aggregate(dat2$biomass,list(dat2$species,dat2$time),sum)
	names(totB) <- c("species","time","biomass")

	#add observation error
	totBobs <- merge(totB,cv,by="species",all.x=T)
	totBobs$var <- log(totBobs$cv^2+1)
	totBobs$obsBiomass <- rlnorm(nrow(totBobs), log(totBobs$biomass)-totBobs$var/2, sqrt(totBobs$var))


	#THIS CODE BELOW IS A METHOD TO CALCULATE BIOMASS PER POLYGON (needs testing)
	#totBobs is the total biomass for a species aggregated over boxes
	#split that into boxes using the true proportion of biomass in each box

	#now split that into the boxes based on the true proportion of biomass by species in each box
	#sum over boxes and ages (the sampled boxes were already subset in create functions)
	# Bbox <- aggregate(dat2$biomass,list(dat2$species,dat2$polygon),sum)
	# names(Bbox) <- c("species","polygon","biomassBox")

	# Bbox2 <- merge(Bbox,totB,by="species",all.x=TRUE)
	# Bbox2$propB <- Bbox2$biomassBox/Bbox2$biomass

	# Bbox3 <- merge(Bbox2,totBobs,by="species",all.x=TRUE)
	# Bbox3$obsBbox <- Bbox3$obsBiomass*Bbox3$propB

	#output (observed biomass by box, which adds up to the appropriate total biomass)
	# out <- data.frame(species=Bbox3$species,
	# 	              agecl = NA,
	# 	              polygon=Bbox3$polygon,
	# 	              layer=NA, time=Bbox3$time,
	# 	              atoutput=Bbox3$obsBbox)

	out <- data.frame(species=totBobs$species,
		              agecl = NA,
		              polygon=NA,
		              layer=NA, time=totBobs$time,
		              atoutput=totBobs$obsBiomass)

    return(out)
}




if(F) {
	#to check how multiple sampling can work
	a <- seq(10,100,10)
	b <- seq(0.1,1,0.1)

	x <- matrix(rlnorm(length(a)*1000,log(a)-(b^2)/2,b),ncol=10,byrow=T)
	apply(x,2,mean)


	directory <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
	scenario <- "SETAS"
	groups <- load_fgs(dir = directory, "functionalGroups.csv")
	groups <- groups[groups$IsTurnedOn > 0, "Name"]
	results <- run_truth(scenario = scenario,
	dir = directory,
	file_fgs = "functionalGroups.csv",
	file_bgm = "VMPA_setas.bgm",
	select_groups = groups,
	file_init = "INIT_VMPA_Jan2015.nc",
	file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")

	species=c("Pisciv_T_Fish","Pisciv_S_Fish")
    boxes <- 1:3
	effic <- data.frame(species=c("Pisciv_T_Fish","Pisciv_S_Fish"), efficiency=c(0.3,0.1))
	selex <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_S_Fish",10)),
	                agecl=c(1:10,1:10),
	                selex=c(0,0,0.1,0.5,0.8,1,1,1,1,1,0,0,0.1,0.3,0.5,0.7,0.9,1,1,1))

	tmp <- create_survey(dat=results$nums, time=seq(70,82,3), species=species, boxes=boxes, effic=effic, selex=selex)

	wtAtAge <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_S_Fish",10)),
	                agecl=c(1:10,1:10),
	                wtAtAge=c(0.1,0.5,0.9,1.5,1.8,2.0,2.1,2.2,2.3,2.35,0.05,0.2,0.3,0.35,0.45,0.5,0.55,0.6,0.63,0.65))
	cv <- data.frame(species=species, cv=c(0.2,0.3))

	survObsBiom <- sample_survey_biomass(dat=tmp,cv=cv,wtAtAge)

}
