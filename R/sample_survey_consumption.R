#' @title Sample total consumption by predator from an atlantis scenario
#'
#' @description The function takes consumption-at-age data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{\link{load_nc}}
#'   within \code{\link{run_truth}}. One does not need to use these functions
#'   to create \code{dat}, rather you must only ensure that the structure of
#'   \code{dat} is the same.
#' @details
#'   This function simply calculates consumption-at-age in t by
#'   summing total predator consumption over polygons,
#'   and then applies user defined error to the consumption.
#'   The result is a coastwide consumption estimate in tons from the survey
#'   Improvements could be to provide polygon specific consumption,
#'   but the cv will need to be thought about.
#' @author Sarah Gaichas
#' @export
#'
#' @template dat
#' @param cv      Coefficient of variation for total predator consumption
#'                    a matrix with columns: species, cv
#'
#' @return The standard dataframe as specified used in \code{dat}.
#'   The function sums over layers and makes \code{$layers} is {NA}.
#'
#' @examples
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' species <- c("Pisciv_T_Fish","Pisciv_S_Fish")
#' truth <- run_truth(scenario = "outputs",
#'   dir = d,
#'   file_fgs = "Functional_groups.csv",
#'   file_bgm = "Geography.bgm",
#'   select_groups = species,
#'   file_init = "Initial_condition.nc",
#'   file_biolprm = "Biology.prm",
#'   file_runprm = "Run_settings.xml")
#'
#'  boxes <- 1:3
#'	effic <- data.frame(species=c("Pisciv_T_Fish","Pisciv_S_Fish"), efficiency=c(0.3,0.1))
#'	selex <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_S_Fish",10)),
#'                agecl=c(1:10,1:10),
#'	                selex=c(0,0,0.1,0.5,0.8,1,1,1,1,1,0,0,0.1,0.3,0.5,0.7,0.9,1,1,1))
#'
#'	tmp <- create_survey(dat=truth$nums, time=seq(10,55,3), species=species, boxes=boxes, effic=effic, selex=selex)
#'
#'	cv <- data.frame(species=species, cv=c(0.2,0.3))
#'
#'	survObsBiom <- sample_survey_biomass(dat=tmp,cv=cv)

sample_survey_consumption <- function(dat,cv) {

	#sum over boxes and ages (the sampled boxes were already subset in create functions)
	totconsB <- aggregate(dat2$biomass,list(dat2$species,dat2$time),sum)
	names(totconsB) <- c("species","time","constons")

	#add observation error
	totconsBobs <- merge(totconsB,cv,by="species",all.x=T)
	totconsBobs$var <- log(totconsBobs$cv^2+1)
	totconsBobs$obsConsB <- rlnorm(nrow(totconsBobs), log(totconsBobs$constons)-totconsBobs$var/2, sqrt(totconsBobs$var))


	out <- data.frame(species=totconsBobs$species,
		              agecl = NA,
		              polygon=NA,
		              layer=NA, time=totconsBobs$time,
		              atoutput=totconsBobs$obsConsB)

    return(out)
}


