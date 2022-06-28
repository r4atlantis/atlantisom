#'Generate predator total consumption index data from atlantisom
#'
#'#'@description A wrapper function to create predator consumption index data for assessment input
#'This is total consumption of all prey combined in tons by predator species.
#'Generates true total consumption and saves as interim step (optionally adds to om_list).
#'Takes the output of \code{om_species}. Wrapper can generate replicates. Saves output as .rds
#'Results for more than one survey are generated with multiple survey config files and
#'saved as separate .rds files.
#'@param usersurvey survey config file in format of /config/usersurvey.R
#'@param userfishery fishery config file in format of /config/fisherycensus.R
#'@param omlist_ss output of \code{om_species}
#'@param n_reps number of replicate indices to be generated
#'@template save
#'@return Returns list objects containing dataframes of survey biomass index and total catch:
#' \itemize{
#'  \item{survObsConsB, list of replicate dataframes of observed survey consumption (tons)}
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
#'CC3om_sard_ind <- om_index(usersurvey = here("config/usersurvey.R"),
#'     userfishery = here("config/fisherycensus.R"),
#'     omlist_ss = CC3om_sardine,
#'     n_reps = 5,
#'     save = TRUE)
#'}
#'
om_consindex <- function(usersurvey = usersurvey_file,
                     omlist_ss,
                     n_reps = n_reps,
                     save = TRUE){

  #one script for dimension parameters to be used in multiple functions
  source("config/omdimensions.R", local = TRUE)

  # user options for survey--default is a census with mid-year sample
  # allows muliple surveys
  survObsConsBs <- list()

  for (s in usersurvey)
  {
    source(s, local = TRUE)

    # what form of survey efficiency and selectivity should be applied to consumption?
    # create_survey function expects NatAge input from aggregateData,
    # which assumes atoutput column needs aggregating, so renamed in calc_pred_cons

    survey_consB <- atlantisom::create_survey(dat = omlist_ss$truecons_ss, #from calc_pred_cons()
                                              time = survtime,
                                              species = survspp,
                                              boxes = survboxes,
                                              effic = surveffic,
                                              selex = survselex)


    # this is the step to repeat n_reps time if we want different realizations
    # of the same survey design specified above; only observation error differs
    # using the census cv of 0 will produce identical reps!
    survObsConsB <- list()
    for(i in 1:n_reps){
      survObsConsB[[i]] <- atlantisom::sample_survey_consumption(survey_consB, cons_cv)
    }

    #save survey indices, takes a long time to generate with lots of reps/species
    if(save){
      saveRDS(survObsConsB, file.path(d.name, paste0(scenario.name, "_",
                                                     survey.name, "survObsConsB.rds")))
    }

    survObsConsBs[[survey.name]] <- survObsConsB
  }


  indices <- list("survObsConsB" = survObsConsBs)

  return(indices)
}
