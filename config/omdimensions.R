#survey species inherited from omlist_ss
survspp <- omlist_ss$species_ss
# survey season and other time dimensioning parameters
# generalized timesteps all models
noutsteps <- omlist_ss$runpar$tstop/omlist_ss$runpar$outputstep
timeall <- c(0:noutsteps)
stepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutinc
midptyr <- round(median(seq(0,stepperyr)))

# model areas, subset in surveyconfig
allboxes <- c(0:(omlist_ss$boxpars$nbox - 1))

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc

# survey selectivity (agecl based)
# should return all age classes fully sampled (Atlantis output is 10 age groups per spp)
n_age_classes <- omlist_ss$funct.group_ss$NumCohorts
age_classes <- 1:n_age_classes

n_annages <- n_age_classes * omlist_ss$funct.group_ss$NumAgeClassSize
annages <- 1:n_annages
