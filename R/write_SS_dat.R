#'@description A wrapper function to read in an SS .dat file, modify it to add a catch vector, survey CPUE, CAAL and length comp data
#' from a survey, and age and length comp data from a fishery using the \code{atlantisom} functions to take data from atlantis.
#'@param model_dir the directory (relative to \inst\extdata) your SS files are in
#'@param datfile_name the name of your stock synthesis .dat file
#'@param survey_ts a data frame with columns \code{atoutput} and \code{time} where atoutput is survey CPUE in biomass (tons)
#'@param fishery_ts a data frame with columns \code{atoutput} and \code{time} where atoutput is fished catch in biomass (tons)
#'@param survey_config the name of your survey config fils
#'@param fishery_config the name of your fishery config file
#'@param fleets a vector corresponding to the index of your fishing fleet followed by the index of your survey fleet
#'@param survey_lencomp output from the atlantisom \code{calc_age2length()} function for the survey
#'@param fishery_lencomp output from the atlantisom \code{calc_age2length()} function for the fishery
#'@return stocksynthesis.data the dat file list object

write_SS_dat <- function(model_dir, datfile_name, survey_ts, fishery_ts, survey_config, fishery_config, fleets = c(2,1),
                         survey_lencomp, fishery_lencomp){

  #Load in config objects
  load(survey_config)
  load(fishery_config)

  surv_vars_exist <- any(c(is.null(CVs),is.null(survey_years),is.null(survey_sample_time),is.null(survey_month)))

  #Check for config objects
  if(!surv_vars_exist){
    stop("You are missing survey configuration objects; did you correctly load your survey config file?")
  }

  if(any(c(is.null(fishing_month),is.null(fish_years)))){
    stop("You are missing fishing configuration objects; did you correctly load your fishing config file?")
  }

  #Rename sampled data
  len_comp_data <- survey_lencomp$natlength
  fish_len_comp_data <- fishery_lencomp$natlength

  #Read in SS .dat file
  stocksynthesis.data <- r4ss::SS_readdat_3.30(paste0("./inst/extdata/",
                                                      model_dir,
                                                      datfile_name))

  #Write CPUE in biomass for one survey and one fishery
  stocksynthesis.data <- SS_write_ts(ss_data_list = stocksynthesis.data,
                                     ts_data = list(survey_ts$atoutput[survey_ts$time %in% survey_years],
                                                    fishery_ts$atoutput[fishery_ts$time %in% fish_years]),
                                     CVs = c(CVs$survey,
                                             CVs$fishery),
                                     data_years = list((survey_ts$time[survey_years]-survey_sample_time)/timestep+1,
                                                       fish_years),
                                     sampling_month = list(rep(survey_month,nyears),
                                                           rep(fishing_month,nyears)),
                                     units = c("biomass","biomass"),
                                     data_type=c("CPUE","catch"),
                                     fleets = fleets)

  ###################################################
  # This section formats the age composition and CAAL data
  ###################################################

  #Get the SS age bins
  age_bin_names <- names(stocksynthesis.data$agecomp)[10:length(names(stocksynthesis.data$agecomp))]
  age_bins <- sub("a","",age_bin_names)

  #Flatten age comps
  age_comp_flat <- reformat_compositions(age_comp_data,
                                         round.places = 4,
                                         comp_type = "agecomp")

  age_comp_flat <- filter(age_comp_flat, (time>burnin*timestep)&(time<(nyears+burnin)*timestep))

  if(!all(age_bins ==names(age_comp_flat)[!names(age_comp_flat) %in% c("time","nsamp")])){
    stop("Age bins from SS dat file do not match the number of age bins generated from the data")
  }

  #Get the SS length bins
  length_bin_names <- names(stocksynthesis.data$lencomp)[7:length(names(stocksynthesis.data$lencomp))]
  length_bins <- sub("l","",length_bin_names)

  #Flatten CAAL comps
  caal_comp_flat <- reformat_compositions(len_comp_data, round.places=4,
                                          comp_type="caalcomp")
  caal_comp_flat <- filter(caal_comp_flat, (time>burnin*timestep)&(time<(nyears+burnin)*timestep))

  if(!all(names(caal_comp_flat)[!names(caal_comp_flat) %in% c("time","nsamp","lower.bins","upper.bins")]==age_bins)){
    stop("Conditional age-at-length bins from atlantis don't match number of age bins in SS file")
  }


  #############################################################################
  # This section creates the length compositions
  #######################################################

  #Add over age classes to get sample size
  len_comp_flat <- reformat_compositions(survey_lencomp,
                                         round.places = 0,
                                         comp_type="lencomp")
  #remove burnin
  len_comp_final <- filter(len_comp_flat,
                           time %in% survey_years)

  #Add over fishery age classes to get length comps
  fish_len_comp_flat <- reformat_compositions(fishery_lencomp,
                                              round.places = 0,
                                              comp_type="lencomp")

  #remove burnin
  fish_len_comp_final <- filter(fish_len_comp_flat,
                                time %in% fish_years)


  #Stick all the comp matrices together into a list
  comp_list <- list(caal_comp_flat, len_comp_final, age_comp_flat, fish_len_comp_final)

  #Get the sampling months repeated in the right way
  apply_month <- list(rep(survey_month, nrow(comp_list[[1]])),
                      rep(survey_month, nrow(comp_list[[2]])),
                      rep(fishing_month,nrow(comp_list[[3]])),
                      rep(fishing_month,nrow(comp_list[[4]])))


  # Write CAAL and length composition data
  stocksynthesis.data <- SS_write_comps(ss_data_list = stocksynthesis.data,
                                        comp_matrix = comp_list,
                                        data_rows = list((comp_list[[1]]$time-survey_sample_time)/timestep + 1 ,
                                                         (survey_years-survey_sample_time)/timestep + 1,
                                                         fish_years,fish_years),
                                        sampling_month = apply_month,
                                        data_type = rep(c("agecomp", "lencomp"),2),
                                        fleet_number = rep(fleets,each=2),
                                        bins = list(age_bins,
                                                    length_bins,
                                                    age_bins,
                                                    length_bins),
                                        caal_bool = c(TRUE, rep(FALSE,3)))

  #Change length bin structure to match atlantis data
  stocksynthesis.data$lbin_vector <- length_bins

  #Get correct number of length bins
  stocksynthesis.data$N_lbins <- length(length_bins)

  #Set lbin_method to 2 - this makes the population length bins set by the user
  #Set the binwidth to be the same as the length bins
  stocksynthesis.data$lbin_method <- 2

  #Double check there aren't different length bin widths
  if(all.equal(diff(length_bins))){
    bin_width <- diff(length_bins)[1]
  } else{
    stop("Error: your data have length bins of differing widths!")
  }
  stocksynthesis.data$binwidth <- bin_width

  #defaults to a minimum bin size of 6 cm for population bins
  stocksynthesis.data$minimum_size <- min(min(length_bins),6)
  stocksynthesis.data$maximum_size <- min(max(length_bins),150)

  #Change minimum sample size to 0.001 for CAAL data (SS won't let it go lower than this)
  stocksynthesis.data$age_info$minsamplesize <- rep(0.001,2)

  #Make sure tail compression is off
  stocksynthesis.data$age_info$CompressBins <- rep(0,length(stocksynthesis.data$age_info$CompressBins))
  stocksynthesis.data$len_info$CompressBins <- rep(0,length(stocksynthesis.data$len_info$CompressBins))

  #Write all changes to the dat file
  SS_writedat_3.30(stocksynthesis.data, outfile = paste0("./inst/extdata/",model_dir,
                                                         datfile_name,"_atlantisom"))

  return(stocksynthesis.data)
}
