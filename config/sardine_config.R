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

#Number of years of data to pull
nyears <- 50

#Atlantis initialization period
burnin <- 30

#Maximum size of a fish in cm
maxbin <- 150

#Years to fish, assuming fishing output is annual
fish_years <- burnin:(burnin+nyears-1)

#Years to survey
survey_years <- survey_sample_full[burnin:(burnin+nyears-1)]

#Month of survey/fishing
survey_month <- 7
fishing_month <- 1

