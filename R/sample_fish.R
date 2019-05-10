#' @title Sample numbers-at-age to create composition data
#'
#' @description Create sampled data from numbers-at-age data to create
#'   composition data from and Atlantis scenario by default.
#'   todo: add more information here later
#'
#' @details The function takes numbers-at-age data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{\link{load_nc}}
#'   within \code{\link{run_truth}}. One does not need to use these functions
#'   to create \code{dat}, rather you must only ensure that the structure of
#'   \code{dat} is the same.
#'   Currently, the function creates sampled numbers-at-age data to create
#'   composition data (age, convert to length, age-at-weigth, etc.).
#'   sums across boxes to properly weight the
#'   numbers-at-age data. Subsequently, the data is sampled using a multinomial
#'   with the effective sample size based on the number of fish you pass in
#'   \code{dat} and \code{prop}. Density data are also needed for biological sampling.
#'   Therefore, when called with sample=FALSE for density data,
#'   applies median to aggregate and does not apply multinomial.
##'   The function starts with numbers-at-age data and therefore the function then must
##'   sum across boxes to properly weight them. Subsequently, the function then uses
##'   a multinomial with a specified effective sample size.
#'   The function could be improved by adding an argument to create spatial
#'   strata which are conglomerations of subsets of boxes.

#' @author Poseidon
#' @export

#' @template dat
#' @param effN    Efficiency for each species: a matrix with nrow=length(species). Columns:
#'                 species:  the species name. Matches names in species
#'                 effN:     the effective N for each species (effective sample size)
#' @param sample  Logical asking whether to apply multinomial sampling using effN.
#' Setting to false results in simple aggregation of atoutput to annual age class values.
#' The default value is \code{TRUE}.
#' @examples
#' 		setwd(file.path(system.file( package = "atlantisom"),".."))
#'
#'		directory <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#'		scenario <- "SETAS"
#'		groups <- load_fgs(dir = directory, "functionalGroups.csv")
#'		groups <- groups[groups$IsTurnedOn > 0, "Name"]
#'		results <- run_truth(scenario = scenario,
#'		dir = directory,
#'		file_fgs = "functionalGroups.csv",
#'		file_bgm = "VMPA_setas.bgm",
#'		select_groups = groups,
#'		file_init = "INIT_VMPA_Jan2015.nc",
#'		file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")
#'
#'		species=c("Pisciv_T_Fish","Pisciv_V_Fish")
#'	    boxes <- 1:3
#'		effic <- data.frame(species=c("Pisciv_T_Fish","Pisciv_V_Fish"), efficiency=c(0.3,0.1))
#'		selex <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_V_Fish",10)),
#'		                agecl=c(1:10,1:10),
#'		                selex=c(0,0,0.1,0.5,0.8,1,1,1,1,1,0,0,0.1,0.3,0.5,0.7,0.9,1,1,1))
#'
#'		tmp <- create_survey(dat=results$nums, time=seq(70,82,3), species=species, boxes=boxes, effic=effic, selex=selex)
#'		effN <- data.frame(species=species, effN=c(200, 500))
#'		samp <- sample_fish(tmp, effN=effN)
#'

sample_fish <- function(dat, effN, sample = TRUE) {

    #assumes that time is already selected/subsetted
	#assumes equal effort in each polygon, thus samples coastwide
	#### Therefore make sure that density is not applied in create_survey function

	#TODO: parameterize effN to vary with time period
  if(sample){
    #sum over boxes and assume sampling occurs coastwide (the sampled boxes were already subset in create functions)
    dat2 <- aggregate(dat$atoutput,list(dat$species,dat$agecl,dat$time),sum)
    names(dat2) <- c("species","agecl","time","numAtAge")

    #TODO: Need error checking--SKG moved nn assignement of effN to inside y loop from outside
	  dat2$numAtAgeSamp <- NA
	  for(sp in unique(dat2$species)) {
	    for(y in unique(dat2$time)) {
	      nn <- effN[effN$species==sp,"effN"]
	      ind <- dat2$species == sp & dat2$time == y
	      totalNums <- sum(dat2[ind,]$numAtAge)

	      if(nn > totalNums) {
	        nn <- totalNums
	        message("effN is greater than total numbers available, so nEff set equal to ", nn," for species ",sp," and time ",y,"\n")
	      }
	      probs <- matrix(dat2[ind,]$numAtAge,nrow=1)
	      if(nn > 0){
	        dat2[ind,]$numAtAgeSamp <- rmultinom(1,nn,probs)[,1]
	      } else { # sample is 0 if probs vector all 0s, no fish that year
	        dat2[ind,]$numAtAgeSamp <- rep(0, length(probs))
	        message("total numAtAge ", nn,", assigning 0 sample for species ",sp," and time ",y,"\n")
	      }
	    }
	  }
	} else {
	  dat2 <- aggregate(dat$medatoutput,list(dat$species,dat$agecl,dat$time),median)
	  names(dat2) <- c("species","agecl","time","medatoutput")
	  dat2$numAtAgeSamp <- dat2$medatoutput
	}

	#output same general format that can be input into comp functions
	out <- data.frame(species = dat2$species,
					  agecl = dat2$agecl,
					  polygon = NA,
					  layer = NA,
					  time = dat2$time,
					  atoutput = dat2$numAtAgeSamp)

	return(out)
}


if(F) {
		dat <- data.frame(species = c(rep("spec1",3*3),rep("spec2",5*3)),
		              agecl = c(rep(1:3,3),rep(3:7,3)),
		              polygon = c(rep(1:3,each=3),rep(1:3,each=5)),
		              layer = 1:2,
		              time = 365)
    dat$atoutput <- 10000/dat$agecl

	dat2 <- dat; dat2$time=365*2;
	dat2$atoutput <- 20000/dat2$agecl
	dat <- rbind(dat,dat2)


    spex <- data.frame(polygon=1:3,area=c(1000,2000,3000))

    boxes <- data.frame(polygon=1:2,survArea=c(10,200))

	effic <- data.frame(species=c("spec1","spec2"), efficiency=c(0.1,0.5))

	selex <- data.frame(species=c(rep("spec1",3),rep("spec2",7)),
		                agecl=c(1:3,1:7),
		                selex=c(0.1,0.5,1,0,0.1,0.3,0.5,0.7,1,1))


	#tmp <- create_survey(dat=dat, time=c(365,2*365), species=c("spec1","spec2"), spex=spex, boxes=boxes, effic=effic, selex=selex)
	tmp <- create_survey(dat=dat, time=c(365,2*365), species=c("spec1","spec2"), effic=effic, selex=selex)

	effN <- data.frame(species=c("spec1","spec2"), effN=c(200, 500))
	#wts number of samples is assumed proportional to area surveyed. In other words, it assumes that a single survey sample is a specific size of area sampled.

	samp <- sample_fish(tmp, effN=effN)
	tapply(samp$atoutput,list(samp$species,samp$time),sum)



		#load_all()
		#devtools::load_all(pkg="C:/GitHub/atlantisom") #A devtools function
		setwd(file.path(system.file( package = "atlantisom"),".."))

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


		#allboxes <- load_box(dir = directory, file_bgm = "VMPA_setas.bgm")
		#boxes <- get_boundary(allboxes)


		#spex <- load_boxarea(dir = directory, file_bgm = "VMPA_setas.bgm")
		species=c("Pisciv_T_Fish","Pisciv_V_Fish")
	    boxes <- 1:3
		effic <- data.frame(species=c("Pisciv_T_Fish","Pisciv_V_Fish"), efficiency=c(0.3,0.1))
		selex <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_V_Fish",10)),
		                agecl=c(1:10,1:10),
		                selex=c(0,0,0.1,0.5,0.8,1,1,1,1,1,0,0,0.1,0.3,0.5,0.7,0.9,1,1,1))


		tmp <- create_survey(dat=results$nums, time=seq(70,82,3), species=species, boxes=boxes, effic=effic, selex=selex)
		effN <- data.frame(species=species, effN=c(200, 500))
		samp <- sample_fish(tmp, effN=effN)

}

