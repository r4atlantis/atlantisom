#' @title Sample a fishery catch index from an atlantis scenario
#'
#' @description The function takes total catch data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{\link{load_catch}}
#'   function. One does not need to use these functions
#'   to create \code{dat}, rather you must only ensure that the structure of
#'   \code{dat} is the same.
#' @details
#'   This function simply creates an observed total catch time series from
#'   total catch input using by applying a user-specified cv.
#'   The result is a coastwide catch (tons) estimate in tons from the fishery.
#' @author Poseidon
#' @export
#'
#' @template dat
#' @param cv      Coefficient of variation for the entire species specific catch
#'                    a matrix with columns: species, cv
#'
#' @return The standard dataframe as specified used in \code{dat}.
#'


sample_fishery_totcatch <- function(dat,cv) {

  #dat is the output of load_catch
	#add observation error
  totCobs <- merge(dat,cv,by="species",all.x=T)
  totCobs$var <- log(totCobs$cv^2+1)
  totCobs$obsCatch <- rlnorm(nrow(totCobs), log(totCobs$atoutput)-totCobs$var/2, sqrt(totCobs$var))

	out <- data.frame(species=totCobs$species,
		              agecl = NA,
		              polygon=NA,
		              layer=NA, time=totCobs$time,
		              atoutput=totCobs$obsCatch)

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
