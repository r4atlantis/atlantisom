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
sp_age <- omlist_ss$funct.group_ss[, c("Name", "NumCohorts", "NumAgeClassSize")]

# should return all age classes fully sampled (Atlantis output is 10 age groups per spp)
n_age_classes <- sp_age$NumCohorts
# changed below for multiple species NOTE survspp alphabetical; NOT in order of fgs!!
# this gives correct names
age_classes <- sapply(n_age_classes, seq)
names(age_classes)<-sp_age$Name

n_annages <- sp_age$NumCohorts * sp_age$NumAgeClassSize
# changed below for multiple species
annages <- sapply(n_annages, seq)
names(annages)<-sp_age$Name
