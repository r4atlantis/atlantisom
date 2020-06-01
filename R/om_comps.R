#'Generate composition data from atlantisom
#'#'@description A wrapper function to create survey and fishery compositional data for assessment input.
#'Takes the output of \code{om_species}. Wrapper can generate replicates. Saves output as .rds
#'@param usersurvey survey config file in format of /config/usersurvey.R
#'@param userfishery fishery config file in format of /config/fisherycensus.R
#'@param omlist_ss output of \code{om_species}
#'@param n_reps number of replicate age, length, and weight-at-age compositions to be generated
#'@template save
#'@return Returns list objects containing dataframes of survey biomass index and total catch:
#' \itemize{
#'  \item{survObsBiomB, list of replicate dataframes of observed survey biomass (tons)}
#'  \item{fishObsCatchB, list of replicate dataframes of observed fishery catch (tons)}
#'  \item{survObsAgeComp, list of replicate dataframes of observed survey age comp (n fish)}
#'  \item{survObsLenComp, list of replicate dataframes of observed survey length comp (n fish)}
#'  \item{survObsWtAtAge, list of replicate dataframes of observed survey weight at age (n fish)}
#'  \item{fishObsAgeComp, list of replicate dataframes of observed fishery age comp (n fish)}
#'  \item{fishObsLenComp, list of replicate dataframes of observed fishery length comp (n fish)}
#'  \item{fishObsWtAtAge, list of replicate dataframes of observed fishery weight at age (n fish)}
#' },
#'
#'@export
#'
#'@family wrapper functions
#'@author Sarah Gaichas
#'
#'@examples
#' # assuming CC3om is output of om_init(here("config/CC3config.r"))
#' # and CC3om_sardine <- om_species(c("Pacific_sardine"), CC3om)
#'
#'CC3om_sard_comp <- om_comps(usersurvey = here("config/usersurvey.R"),
#'     userfishery = here("config/fisherycensus.R"),
#'     omlist_ss = CC3om_sardine,
#'     n_reps = 1,
#'     save = TRUE)
#'
#'
om_comps <- function(usersurvey = usersurvey_file,
                     userfishery = userfishery_file,
                     omlist_ss,
                     n_reps = n_reps,
                     save = TRUE){

  #one script for dimension parameters to be used in multiple functions
  source("config/omdimensions.R", local = TRUE)

  # user options for survey--default is a census with mid-year sample
  source(usersurvey, local = TRUE)

  #numbers based fishery independent survey for age and length comps
  # same user specifications as indices
  survey_N <- atlantisom::create_survey(dat = omlist_ss$truenums_ss,
                                        time = survtime,
                                        species = survspp,
                                        boxes = survboxes,
                                        effic = surveffic,
                                        selex = survselex)

  #Sample fish for age composition
  # if we want replicates for obs error this sample function will generate them
  age_comp_data <- list()
  for(i in 1:n_reps){
    age_comp_data[[i]] <- atlantisom::sample_fish(survey_N, surveffN)
  }

  # save age comps
  if(save){
    saveRDS(age_comp_data, file.path(d.name, paste0(scenario.name, "survObsAgeComp.rds")))
  }

  #weights needed for weight at age and length comp calcs
  # aggregate true resn per survey design
  survey_aggresn <- atlantisom::aggregateDensityData(dat = omlist_ss$trueresn_ss,
                                                     time = survtime,
                                                     species = survspp,
                                                     boxes = survboxes)

  # aggregate true structn per survey design
  survey_aggstructn <- atlantisom::aggregateDensityData(dat = omlist_ss$truestructn_ss,
                                                        time = survtime,
                                                        species = survspp,
                                                        boxes = survboxes)

  #dont sample these, just aggregate them using median
  structnss <- atlantisom::sample_fish(survey_aggresn, surveffN, sample = FALSE)

  resnss <- atlantisom::sample_fish(survey_aggstructn, surveffN, sample = FALSE)

  #this is all input into the length function, replicates follow age comp reps
  #  separating the length comps from the weight at age here
  survey_lenwt <- list()
  survObsLenComp <- list()
  survObsWtAtAge <- list()

  for(i in 1:n_reps){
    survey_lenwt[[i]] <- atlantisom::calc_age2length(structn = structnss,
                                                     resn = resnss,
                                                     nums = age_comp_data[[i]],
                                                     biolprm = omlist_ss$biol,
                                                     fgs = omlist_ss$funct.group_ss,
                                                     maxbin = maxbin,
                                                     CVlenage = lenage_cv,
                                                     remove.zeroes=TRUE)

    survObsLenComp[[i]] <- survey_lenwt[[i]]$natlength
    survObsWtAtAge[[i]] <- survey_lenwt[[i]]$muweight
  }

  if(save){
    saveRDS(survObsLenComp, file.path(d.name, paste0(scenario.name, "survObsLenComp.rds")))
    saveRDS(survObsWtAtAge, file.path(d.name, paste0(scenario.name, "survObsWtAtAge.rds")))
  }

  #now do fishery comps
  # user options for fishery--default is a census with mid-year sample
  source(userfishery, local = TRUE)

  #fishery catch at age each observed timestep summed over observed polygons
  # catch at age by area and timestep
  catch_numbers <-  atlantisom::create_fishery_subset(dat = omlist_ss$truecatchnum_ss,
                                                      time = fishtime,
                                                      species = survspp,
                                                      boxes = fishboxes)

  # if we want replicates for obs error this sample function will generate them
  catch_age_comp <- list()
  for(i in 1:n_reps){
    catch_age_comp[[i]] <- atlantisom::sample_fish(catch_numbers, fisheffN)
  }

  # save fishery age comps
  if(save){
    saveRDS(catch_age_comp, file.path(d.name, paste0(scenario.name, "fishObsAgeComp.rds")))
  }

  #Get catch weights for length comp calc
  # aggregate true resn per fishery subset design
  catch_aggresnss <- atlantisom::aggregateDensityData(dat = omlist_ss$trueresn_ss,
                                                      time = fishtime,
                                                      species = survspp,
                                                      boxes = fishboxes)

  # aggregate true structn fishery subsetdesign
  catch_aggstructnss <- atlantisom::aggregateDensityData(dat = omlist_ss$truestructn_ss,
                                                         time = fishtime,
                                                         species = survspp,
                                                         boxes = fishboxes)

  #dont sample these, just aggregate them using median
  catch_structnss <- atlantisom::sample_fish(catch_aggstructnss, fisheffN, sample = FALSE)

  catch_resnss <- atlantisom::sample_fish(catch_aggresnss, fisheffN, sample = FALSE)

  # these fishery lengths and weight at age are each output timestep
  #same structure as above for surveys, replicates follow age comp reps
  #  separating the length comps from the weight at age here
  fishery_lenwt <- list()
  fishObsLenComp <- list()
  fishObsWtAtAge <- list()

  for(i in 1:n_reps){
    fishery_lenwt[[i]] <- atlantisom::calc_age2length(structn = catch_structnss,
                                                      resn = catch_resnss,
                                                      nums = catch_age_comp[[i]],
                                                      biolprm = omlist_ss$biol,
                                                      fgs = omlist_ss$funct.group_ss,
                                                      maxbin = maxbin,
                                                      CVlenage = lenage_cv,
                                                      remove.zeroes=TRUE)

    fishObsLenComp[[i]] <- fishery_lenwt[[i]]$natlength
    fishObsWtAtAge[[i]] <- fishery_lenwt[[i]]$muweight
  }

  if(save){
    saveRDS(fishObsLenComp, file.path(d.name, paste0(scenario.name, "fishObsLenComp.rds")))
    saveRDS(fishObsWtAtAge, file.path(d.name, paste0(scenario.name, "fishObsWtAtAge.rds")))
  }

  if(!is.null(omlist_ss$truenumsage_ss)){
    #numbers based fishery independent survey for age and length comps
    # same user specifications as indices
    survey_annageN <- atlantisom::create_survey(dat = omlist_ss$truenumsage_ss,
                                          time = survtime,
                                          species = survspp,
                                          boxes = survboxes,
                                          effic = surveffic,
                                          selex = survselex)
    #Sample fish for age composition
    # if we want replicates for obs error this sample function will generate them
    annage_comp_data <- list()
    for(i in 1:n_reps){
      annage_comp_data[[i]] <- atlantisom::sample_fish(survey_annN, surveffN)
    }

    # save survey annual age comps
    if(save){
    saveRDS(annage_comp_data, file.path(d.name, paste0(scenario.name, "survObsFullAgeComp.rds")))
    }

  }else{annage_comp_data <- NULL}

  if(!is.null(omlist_ss$truecatchage_ss)){
    #numbers based fishery independent survey for age and length comps
    # same user specifications as indices
    #fishery catch at age each observed timestep summed over observed polygons
    # catch at age by area and timestep
    catch_annagenumbers <-  atlantisom::create_fishery_subset(dat = omlist_ss$truecatchage_ss,
                                                        time = fishtime,
                                                        species = select_groups,
                                                        boxes = fishboxes)

    # if we want replicates for obs error this sample function will generate them
    # NOTE THIS AGGREGATES ACROSS FLEETS need to change sample_fish to fix
    catch_annage_comp <- list()
    for(i in 1:n_reps){
      catch_annage_comp[[i]] <- atlantisom::sample_fish(catch_numbers, fisheffN)
    }

    # save fishery annual age comps
    if(save){
    saveRDS(catch_annage_comp, file.path(d.name, paste0(scenario.name, "fishObsFullAgeComp.rds")))
    }
  }else{catch_annage_comp <- NULL}

  # call interpolate weight at age function to get survObsFullWtAtAge


  comps <- list("survObsAgeComp" = age_comp_data,
                "survObsLenComp" = survObsLenComp,
                "survObsWtAtAge" = survObsWtAtAge,
                "fishObsAgeComp" = catch_age_comp,
                "fishObsLenComp" = fishObsLenComp,
                "fishObsWtAtAge" = fishObsWtAtAge,
                "survObsFullAgeComp" = annage_comp_data,
                "fishObsFullAgeComp" = catch_annage_comp
                )

  return(comps)
}
