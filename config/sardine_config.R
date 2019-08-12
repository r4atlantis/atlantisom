#Read in data file

#Set species name
species <- "Pacific_sardine"

#Directory with SS files
model_dir <- "Sardine_SS_files/"

#Name of SS data file
datfile_name <- "sardEM_3_3.dat"

#Name of atlantis model output - this is an RData object from the runtruth() function
atlantis_output_name <- "outputCCV3run_truth.RData"

#Efficiency
eff_case <- 0.5

#Age classes of your species
age_classes <- 1:10

#Selectivity function - a vector equivalent to length (age classes)
sel_by_age <- rep(1,length(age_classes))

#Atlantis model timestep corresponding to the true output
timestep <- 5

#Which atlantis timestep does the survey run in?
survey_sample_time <- 3

#The last timestep to sample
total_sample <- 495

#Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
                          total_sample, by=timestep)

#Effective sample size for composition data
surveyEffN <- 1000
fisheryEffN <- 1000

#CVs for length at age, catch, and survey
CVs <- list("lenage"=0.1, "fishery"=0.01, "survey"=0.1)

#Efficiency
eff_case <- 0.5

#Age classes of your species
age_classes <- 1:10

#Selectivity function - a vector equivalent to length (age classes)
sel_by_age <- rep(1,length(age_classes))

#Atlantis model timestep corresponding to the true output--now from census_spec.R
timestep <- stepperyr #5

#Which atlantis timestep does the survey run in?--now from census_spec.R
# with 5 output steps per year, 0 is Jan-Feb-midMar, 1 is midMar-Apr-May,
# 2 is June-July-midAug, 3 is midAug-Sept-Oct, 4 is Nov-Dec (ish)

survey_sample_time <- midptyr #3 this is more late summer-fall, changed to 2

#The last timestep to sample
total_sample <- noutsteps-1 #495

#Vector of indices of survey times to pull
survey_sample_full <- seq(survey_sample_time,
                          total_sample, by=timestep)

#Effective sample size for composition data (annual total samples)
surveyEffN <- 1000
fisheryEffN <- 1000

#CVs for length at age, catch, and survey
CVs <- list("lenage"=0.1, "fishery"=0.01, "survey"=0.1)

#Number of years of data to pull
nyears <- 50

#Atlantis initialization period in years
burnin <- 30

#Maximum size of a fish in cm
maxbin <- 150

#Years to fish, assuming fishing output is annual and years index 1,2,3 etc
#fish_years <- burnin:(burnin+nyears-1)
# fishery output timestep (fstepperyr in census_spec.R) for this run is 5 per year

# here we are summing the catch in numbers for input in to SS.
# To use the total catch biomass output from catch.txt the above would work
# because the output is annual but in units of DAYS so needs time/365

#The last timestep to sample
total_sample <- noutsteps-1 #495

#Vector of indices of catch in numbers to pull (by timestep to sum)
fish_sample_full <- c(0:total_sample)
fish_burnin <- burnin*fstepperyr+1
fish_nyears <- nyears*fstepperyr
fish_times <- fish_sample_full[fish_burnin:(fish_burnin+fish_nyears-1)]
fish_timesteps <- seq(fish_times[5], max(fish_times), by=fstepperyr) #last timestep
fish_years <- unique(floor(fish_times/fstepperyr))



#Years to survey
survey_years <- survey_sample_full[burnin:(burnin+nyears-1)]

#Month of survey/fishing
survey_month <- 7
fishing_month <- 1

