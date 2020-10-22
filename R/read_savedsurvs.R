#'Read saved survey outputs into R object
#'@description A function to read survey index and composition data saved as .rds into
#'an R object with the same structure as the output of \code{om_species} or \code{om_comps}.
#'Users must enter the path where the survey output files are stored and the type of survey
#'output file to be read in.
#'
#' @template dir
#' @template type
#'
#' @details \code{om_species} and \code{om_comps} allow for multiple surveys. Users
#' must define a survey.name in each survey config file which is used to name the file
#' that is output by each survey function, and is then read back in to name list
#' output for each survey in the saved .rds file.
#'
#'@export
#'
#'@family read functions
#'@author Sarah Gaichas
#'
#'@examples
#'\dontrun{
#' # assuming d.name is the path where atlantisom output is stored
#'
#'survObsBiom <- read_savedsurvs(d.name, 'survB') # survey biomass index
#'age_comp_data <- read_savedsurvs(d.name, 'survAge') # survey age class composition
#'len_comp_data <- read_savedsurvs(d.name, 'survLen') # survey length composition
#'wtage <- read_savedsurvs(d.name, 'survWtage') # survey weight at age class
#'annage_comp_data <- read_savedsurvs(d.name, 'survAnnAge') # survey annual age composition
#'annage_wtage <- read_savedsurvs(d.name, 'survAnnWtage') # survey weight at annual age
#'
#'}
#'
read_savedsurvs <- function(dir, type){

  datlook <- data.frame(dattype = c('survB', 'survAge', 'survLen', 'survWtage', 'survAnnAge', 'survAnnWtage'),
                        pattern = c("*surveyB.rds", "*survObsAgeComp.rds", "*survObsLenComp.rds", "*survObsWtAtAge.rds",
                                    "*survObsFullAgeComp.rds", "*survObsFullWtAtAge.rds"))

  survs <- list.files(path=dir, pattern = as.character(datlook$pattern[datlook$dattype %in% type]), full.names = TRUE)

  survey.name <-  str_match(survs, paste0(scenario.name,"_\\s*(.*?)\\s",datlook$pattern[datlook$dattype==type]))[,2]

  dat <- lapply(survs, readRDS)

  names(dat) <- survey.name

  return(dat)
}
