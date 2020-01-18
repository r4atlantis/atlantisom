# Default fishery configuration here is a census
# Inherits species from input omlist_ss
fishspp <- omlist_ss$species_ss 

# same time dimensioning parameters as in surveycensus.R
# does the fishery sampling miss any timesteps? default is no
fishtime <- timeall

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc

# fishery sampling area
# should return all model areas, this assumes you see everything that it caught
fishboxes <- c(0:(omlist_ss$boxpars$nbox - 1))

# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
fisheffN <- data.frame(species=survspp, effN=rep(1e+8, length(survspp)))

#this adjusts for subannual fishery output so original effN is for whole year
fisheffN$effN <- fisheffN$effN/fstepperyr 

# fishery catch cv can be used in sample_survey_biomass
# perfect observation
fish_cv <- data.frame(species=survspp, cv=rep(0.0,length(survspp)))

