#'Generate index data from atlantisom
#'
#'#'@description A wrapper function to create survey and fishery index data for assessment input.
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
#'  \item{survObsBiomB, list of replicate dataframes of observed survey biomass (tons)}
#'  \item{fishObsCatchB, list of replicate dataframes of observed fishery catch (tons)}
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
om_index <- function(usersurvey = usersurvey_file,
                     userfishery = userfishery_file,
                     omlist_ss,
                     n_reps = n_reps,
                     save = TRUE){

  #one script for dimension parameters to be used in multiple functions
  source("config/omdimensions.R", local = TRUE)

  # user options for survey--default is a census with mid-year sample
  # allows muliple surveys
  survObsBiomBs <- list()

  for (s in usersurvey)
  {
    source(s, local = TRUE)

    # add in biomass pools if they exist
    if("truebiopool_ss" %in% names(omlist_ss)){
      truebio_ss <- rbind(omlist_ss$truebio_ss, omlist_ss$truebiopool_ss)
    }else{
      truebio_ss <- omlist_ss$truebio_ss
    }

    #biomass based fishery independent survey index
    # this uses result$biomass_ages to sample biomass directly, no need for wt@age est
    survey_B <- atlantisom::create_survey(dat = truebio_ss,
                                          time = survtime,
                                          species = survspp,
                                          boxes = survboxes,
                                          effic = surveffic,
                                          selex = survselex)

    # call sample_survey_biomass with a bunch of 1000s for weight at age
    # in the code it multiplies atoutput by wtatage/1000 so this allows us to use
    # biomass directly
    wtage <- data.frame(species=rep(names(age_classes), n_age_classes),
                        agecl=unlist(sapply(n_age_classes,seq)),
                        wtAtAge=rep(1000.0,sum(n_age_classes)))

    # this is the step to repeat n_reps time if we want different realizations
    # of the same survey design specified above; only observation error differs
    # using the census cv of 0 will produce identical reps!
    survObsBiomB <- list()
    for(i in 1:n_reps){
      survObsBiomB[[i]] <- atlantisom::sample_survey_biomass(survey_B, surv_cv, wtage)
    }

    #save survey indices, takes a long time to generate with lots of reps/species
    if(save){
      saveRDS(survObsBiomB, file.path(d.name, paste0(scenario.name, "_",
                                                     survey.name, "surveyB.rds")))
    }

    survObsBiomBs[[survey.name]] <- survObsBiomB
  }

  #configure the fishery, a default is in config/fisherycensus.R
  #fishery configuration can specify only area and time of observation
  #fishery species inherited from omlist_ss
  #this is total catch not by fleet, so only one "fishery"

  # 2023 update, can now get fleet specific catch by polygon
  # user options for fishery--default is a census in all areas for all fleets
  # allows multiple fisheries
  fishObsCatchBs <- list()

  for(f in userfishery){

    source(f, local = TRUE)

    # 2023 update: we can now subset fishery catch from CATCH.nc using fleet output
    # create_fishery_subset as currently written aggregates across fleets and polygons
    # change so that fleets to be aggregated together are specified in the userfishery file

    fishery_C <- atlantisom::create_fishery_subset(dat = omlist_ss$truecatchtons_ss,
                                                   time = fishtime,
                                                   fleets = fishfleets,
                                                   species = fishspp,
                                                   boxes = fishboxes)


    fishObsCatchB <- list()
    for(i in 1:n_reps){
      fishObsCatchB[[i]] <- atlantisom::sample_fishery_totcatch(fishery_C, fish_cv)
    }

    if(save){
      saveRDS(fishObsCatchB, file.path(d.name, paste0(scenario.name, "_",
                                                      fishery.name, "fishCatch.rds")))
    }

    fishObsCatchBs[[fishery.name]] <- fishObsCatchB

  }

  indices <- list("survObsBiomB" = survObsBiomBs,
                  "fishObsCatchB" = fishObsCatchBs)

  return(indices)
}
