#'Read saved fishery outputs into R object
#'@description A function to read fishery index and composition data saved as .rds into
#'an R object with the same structure as the output of \code{om_species} or \code{om_comps}.
#'Users must enter the path where the fishery output files are stored and the type of fishery
#'output file to be read in.
#'
#' @template dir
#' @template type
#'
#' @details \code{om_species}, \code{om_comps}, and \code{om_diet} will allow for multiple fisheries.
#' Users must define a fishery.name in each fishery config file which is used to name the file
#' that is output by each fishery function, and is then read back in to name list
#' output for each fishery in the saved .rds file.
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
#'catchbio <- read_savedfisheries(d.name, 'Catch') # total fishery catch in tons
#'fish_age_comp_data <- read_savedfisheries(d.name, 'catchAge') # fishery age class composition
#'catchlen <- read_savedfisheries(d.name, "catchLen") # fishery length composition
#'fish)wtage <- read_savedfisheries(d.name, 'catchWtage') # fishery weight at age class
#'fish_annage_comp_data <- read_savedfisheries(d.name, 'catchAnnAge') # fishery annual age composition
#'fish_annage_wtage <- read_savedfisheries(d.name, 'catchAnnWtage') # fishery weight at annual age
#'
#'}
#'
read_savedfisheries <- function(dir, type){

  datlook <- data.frame(dattype = c('Catch', 'catchAge', 'catchLen', 'catchWtage', 'catchAnnAge', 'catchAnnWtage'),
                        pattern = c("*fishCatch.rds", "*fishObsAgeComp.rds", "*fishObsLenComp.rds", "*fishObsWtAtAge.rds",
                                    "*fishObsFullAgeComp.rds", "*fishObsFullWtAtAge.rds"))

  fisheries <- list.files(path=dir, pattern = as.character(datlook$pattern[datlook$dattype %in% type]), full.names = TRUE)

  fishery.name <-  stringr::str_match(fisheries, paste0(scenario.name,"_\\s*(.*?)\\s",datlook$pattern[datlook$dattype==type]))[,2]

  dat <- lapply(fisheries, readRDS)

  names(dat) <- fishery.name

  return(dat)
}
