# should return a perfectly efficient survey 
effic1 <- data.frame(species=funct.group.names,
                     efficiency=rep(1.0,length(funct.group.names)))

# should return all lengths fully sampled (Atlantis output is 10 age groups per spp)
selex1 <- data.frame(species=rep(funct.group.names, each=10),
                     agecl=rep(c(1:10),length(funct.group.names)),
                     selex=rep(1.0,length(funct.group.names)*10))

# should return all model areas
boxpars <- load_box(d.name, box.file)
boxall <- c(0:(boxpars$nbox - 1))

# generalized timesteps all models
runpar <- load_runprm(d.name, run.prm.file)
noutsteps <- runpar$tstop/runpar$outputstep
stepperyr <- if(runpar$outputstepunit=="days") 365/runpar$toutinc
midptyr <- round(median(seq(0,stepperyr)))

# a survey that takes place once per year mid year
annualmidyear <- seq(midptyr, noutsteps, stepperyr)

timeall <- c(0:noutsteps)

# learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(runpar$outputstepunit=="days") 365/runpar$toutfinc

# define set of species we expect surveys to sample (e.g. fish only? vertebrates?)
# for ecosystem indicator work test all species, e.g.
survspp <- funct.group.names 

# for length and age groups lets just do fish and sharks
# NOBA model has InvertType, changed to GroupType in file, but check Atlantis default
if(initNOBA) funct.groups <- rename(funct.groups, GroupType = InvertType)

survspp <- funct.groups$Name[funct.groups$IsTurnedOn==1 &
                               funct.groups$GroupType %in% c("FISH", "SHARK")]

# needed for sample_fish
# this effective N is high but not equal to total for numerous groups
effNhigh <- data.frame(species=survspp, effN=rep(1e+8, length(survspp)))

# needed for sample_survey_xxx
# perfect observation
surv_cv <- data.frame(species=survspp, cv=rep(0.0,length(survspp)))

