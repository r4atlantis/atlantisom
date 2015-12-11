#' @title Add observation error to numbers-at-age data
#'
#' @details The function takes numbers-at-age data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{\link{load_nc}}
#'   within \code{\link{run_atlantis}}. One does not need to use these functions
#'   to create \code{dat}, rather you must only ensure that the structure of
#'   \code{dat} is the same.
#'   Currently, the function sums across boxes to properly weight the
#'   numbers-at-age data. Subsequently, the data is sampled using a multinomial
#'   with the effective sample size based on the number of fish you pass in
#'   \code{dat} and \code{prop}.
#'   Additionally, you can apply ageing error to the data, using the \code{ageErr}
#'   argument.
#' @author Poseidon

#' @param dat 	  The dataframe of numbers-at-age from create_survey or create_fishery_subset
#'                   columns: species, agecl, polygon, layer, time, atoutput
#'                     atoutput is numbers-at-age
#' @param prop    Percentage of samples for each species: a matrix with nrow=length(species). Columns:
#'                 species:  the species name. Matches names in species
#'                 prop:     the percentage of age samples for each species (for example, 01 if 1 out of every 10 lengthed fish is sampled for age). Max of 1.
#' @param ageErr  ageErr is a list with elements containing a matrix for each species
#'					list element name is species name
#'                  matrix rows are true age, columns are assigned age
#'                  species that are missing will be assigned no ageing error (so a value of NULL means no ageing error for all species)
#'
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
#'	    prop <- data.frame(species=species, prop=c(0.5,1)) #should be same as input when prop=1, but with ageing error
#'      sample_ages(samp,prop,ageErr=NULL)

sample_ages <- function(dat,prop,ageErr=NULL) {

#how will a max age be defined for each species. Is this just the max agecl, or will it be something different after appling stage2age

#assumes that input is aggregated over the necessary columns (box, layer)

#need to use output from sample_fish

	dat$numAtAgeSamp <- dat$ageComp <- NA

	out <- data.frame(species = NULL,
						 age = NULL,
						 time = NULL,
						 propAtAge = NULL)

	species <- unique(dat$species)
	for(sp in species) {
		#set up ageing error if defined as none
		if(!(sp%in%names(ageErr))) {
			maxAge <- max(dat$agecl[dat$species==sp])
			ageErr[[sp]] <- diag(x=1, nrow=maxAge, ncol=maxAge)
		}

		#do sampling for each species
		pp <- prop[prop$species==sp,"prop"]
		for(y in unique(dat$time)) {
			ind <- dat$species == sp & dat$time == y

			totalNums <- sum(dat[ind,]$atoutput)
			nn <- pp * totalNums

			#Ageing Error
			ageingError <- ageErr[[sp]]

			#add in missing ages
			dat2 <- merge(dat[ind,],data.frame(agecl=1:max(dat[ind,"agecl"])),by="agecl",all=T)
			dat2$species <- sp
			dat2$time <- y
			dat2$atoutput[is.na(dat2$atoutput)] <- 0

			#want to actually sample from this so that it returns the exact same vector when percentage = 100
			#this could be expanded in the future to actually provide a sample of individual fish
			sampVec <- rep(dat2$agecl,times=dat2$atoutput)
			samp <- sample(sampVec, nn, replace=FALSE)
			sampTable <- table(samp)
			dat2$numAtAgeSamp <- sampTable[match(dat2$agecl, names(sampTable))]
			dat2$numAtAgeSamp[is.na(dat2$numAtAgeSamp)] <- 0
			#probs <- matrix(dat2$atoutput,nrow=1)
		    #dat2$numAtAgeSamp <- (rmultinom(1,nn,probs)[,1]%*%ageingError)[1,]   #this is a quick way to apply ageing error. Could think of each row (true age) as a multinomial sample
		    dat2$ageComp <- dat2$numAtAgeSamp/sum(dat2$numAtAgeSamp)

		    dat3 <- data.frame(species = dat2$species,
							   age = dat2$agecl,
							   time = dat2$time,
			 				   propAtAge = dat2$ageComp)

		    out <- rbind(out,dat3)
		}
	}

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


	prop <- data.frame(species=c("spec1","spec2"), prop=c(0.5,1)) #should be same as input, but with ageing error
    sample_ages(samp,prop,ageErr=NULL)


	setwd(file.path(system.file( package = "atlantisom"),".."))

	directory <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
	scenario <- "SETAS"
	groups <- load_fgs(dir = directory, "functionalGroups.csv")
	groups <- groups[groups$IsTurnedOn > 0, "Name"]
	results <- run_atlantis(scenario = scenario,
	dir = directory,
	file_fgs = "functionalGroups.csv",
	file_bgm = "VMPA_setas.bgm",
	select_groups = groups,
	file_init = "INIT_VMPA_Jan2015.nc",
	file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")

	species=c("Pisciv_T_Fish","Pisciv_V_Fish")
    boxes <- 1:3
	effic <- data.frame(species=c("Pisciv_T_Fish","Pisciv_V_Fish"), efficiency=c(0.3,0.1))
	selex <- data.frame(species=c(rep("Pisciv_T_Fish",10),rep("Pisciv_V_Fish",10)),
	                agecl=c(1:10,1:10),
	                selex=c(0,0,0.1,0.5,0.8,1,1,1,1,1,0,0,0.1,0.3,0.5,0.7,0.9,1,1,1))

	tmp <- create_survey(dat=results$nums, time=seq(70,82,3), species=species, boxes=boxes, effic=effic, selex=selex)
	effN <- data.frame(species=species, effN=c(200, 500))
	samp <- sample_fish(tmp, effN=effN)

    prop <- data.frame(species=species, prop=c(0.5,1)) #should be same as input when prop=1, but with ageing error
    ageSamp <- sample_ages(samp,prop,ageErr=NULL)

}



