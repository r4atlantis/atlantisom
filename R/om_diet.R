#'Generate diet composition data from atlantisom
#'
#'#'@description A wrapper function to create survey diet composition data for model input.
#'Takes the output of \code{om_species}. Wrapper can generate replicates. Saves output as .rds
#'Results for more than one survey are generated with multiple survey config files and
#'saved as separate .rds files.
#'@param config file specifying directory with atlantis model outputs and identifying key files
#'@param dietfile A character value, specifying the file name of the
#'   \code{DetailedDietCheck.txt} output file from Atlantis (or see \code{\link{load_detailed_diet}}).
#'   This file is read with \code{data.table::fread()}, so it can be
#'   compressed with a .gz suffix, or uncompressed as a .txt file.
#'   If \code{is.null(dir)}, then \code{file_diet} can be the full file
#'   path or a file in your current working directory, or the \code{file_diet}
#'   will be appended to \code{dir} using \code{file.path}.
#'@param usersurvey survey config file in format of /config/usersurvey.R
#'@param omlist_ss output of \code{om_species}
#'@param n_reps number of replicate age, length, and weight-at-age compositions to be generated
#'@template save
#'@return Returns a list object containing dataframes of survey diet proportion:
#' \itemize{
#'  \item{survObsDiets, list of replicate dataframes of observed survey diets (proportion)}
#' },
#'
#'@export
#'
#'@family wrapper functions
#'@author Sarah Gaichas
#'
#'@examples
#'\dontrun{
#' # assuming CC3om is output of om_init(here("config/CC3config.r"))
#' # and CC3om_sardine <- om_species(c("Pacific_sardine"), CC3om)
#'
#'CC3om_sard_comp <- om_comps(usersurvey = here("config/usersurvey.R"),
#'     userfishery = here("config/fisherycensus.R"),
#'     omlist_ss = CC3om_sardine,
#'     n_reps = 1,
#'     save = TRUE)
#'
#'}
om_diet<- function(config = configfile,
                   dietfile = file_diet,
                   usersurvey = usersurvey_file,
                   omlist_ss,
                   n_reps = n_reps,
                   save = TRUE){

  source(config)

  #Load functional groups
  fgs <- atlantisom::load_fgs(dir=d.name,
                                       file_fgs = functional.groups.file)

  # load or read in saved detailed diet
  if(!file.exists(file.path(d.name,
                            paste0(scenario.name, "detaileddiet.rds")))){
    detaileddiet <- load_detailed_diet_comp(dir = d.name,
                                            file_diet,
                                            fgs = fgs)

    if(save){
      saveRDS(detaileddiet, file.path(d.name, paste0(scenario.name, "detaileddiet.rds")))
    }

  } else {
    detaileddiet <- readRDS(file.path(d.name,
                                      paste0(scenario.name, "detaileddiet.rds")))
  }

  #one script for dimension parameters to be used in multiple functions
  source("config/omdimensions.R", local = TRUE)

  survObsDiets <- list()

  for (s in usersurvey)
  {
    source(s, local = TRUE)

    # survtime doesn't match units of time.days in detaileddiet
    survtime <- survey_sample_full*omlist_ss$runpar$outputstep

    # apply survey design to detailed diet
    survey_cons <- create_survey_diet(dat = detaileddiet,
                                      time = survtime,
                                      species = survspp,
                                      boxes = survboxes,
                                      effic = surveffic,
                                      selex = survselex)

    # add observation error to survey diet
    survObsDiet <- list()
    for(i in 1:n_reps){
      survObsDiet[[i]] <- atlantisom::sample_diet(survey_cons, fgs, unidprey = unidprey, alphamult = alphamult)
    }

    #save survey diets, takes a long time to generate with lots of reps/species
    if(save){
      saveRDS(survObsDiet, file.path(d.name, paste0(scenario.name, "_",
                                                    survey.name, "surveydiet.rds")))
    }

    survObsDiets[[survey.name]] <- survObsDiet

  }

  return(survObsDiets)

}
