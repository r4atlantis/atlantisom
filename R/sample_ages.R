#sample_ages R function for Atlantis Summitt Poseidon adventure

#' @author Poseidon

#' @param dat 	  The dataframe of numbers-at-age from create_survey or create_fishery_subset
#'                   columns: species, agecl, polygon, layer, time, atoutput
#'                     atoutput is numbers-at-age
#' @param effN    Efficiency for each species: a matrix with nrow=length(species). Columns:
#'                 species:  the species name. Matches names in species
#'                 effN:     the effective N for each species (effective sample size)
#' @param ageErr  ageErr is a list with elements containing a matrix for each species
#'					list element name is species name
#'                  matrix rows are true age, columns are assigned age
#'                  species that are missing will be assigned no ageing error (so a value of NULL means no ageing error for all species)



#' @details create a sampled age composition from numbers-at-age
#' @details because it starts with numbers-at-age, I add across boxes to properly weight them
#' @details then, it simply applies a multinomial with the effective sample size
#' @details it can also apply ageing error


sample_ages <- function(dat,effN,ageErr=NULL) {

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
		nn <- effN[effN$species==sp,"effN"]
		for(y in unique(dat$time)) {
			ind <- dat$species == sp & dat$time == y

			totalNums <- sum(dat[ind,]$atoutput)
			if(nn > totalNums) {
				cat("effN is greater than total numbers available, so nEff set equal from",nn,"to",totalNums,"\n")
				nn <- totalNums
			}
			#Ageing Error
			ageingError <- ageErr[[sp]]

			#add in missing ages
			dat2 <- merge(dat[ind,],data.frame(agecl=1:max(dat[ind,"agecl"])),by="agecl",all=T)
			dat2$species <- sp
			dat2$time <- y
			dat2$atoutput[is.na(dat2$atoutput)] <- 0

			probs <- matrix(dat2$atoutput,nrow=1)
		    dat2$numAtAgeSamp <- (rmultinom(1,nn,probs)[,1]%*%ageingError)[1,]   #this is a quick way to apply ageing error. Could think of each row (true age) as a multinomial sample
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


	effN <- data.frame(species=c("spec1","spec2"), effN=c(100,1000))
    sample_ages(samp,effN,ageErr=NULL)



}



