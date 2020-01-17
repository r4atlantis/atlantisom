#'Function to write an empirical weight at age SS file (wt_at_age.ss) from Atlantis output
#'@description
#'@param age_classes the vector of age classes in the model
#'@param years
#'@param fleets
#'@param seas
#'@param catch_mean_wt_matrix mu_weight object in grams
#'@param surv_mean_wt_matrix mu_weight object in grams
#'@param fleet_month vector of months (1-12) when sampling occurs
#'@author Christine Stawitz

SS_write_EWAA <- function(age_classes, years, fleets, seas, catch_mean_wt_matrix, surv_mean_wt_matrix, fleet_month, mat_vector){

  #First value in empirical weight at age file is the maximum age; use this
  maximum_age <- max(age_classes)

  #Populate empty data frame
  new_df <- expand.grid(Yr=years, Seas=seas, Sex = 1, BioPattern = 1, BirthSeas = 1,Fleet=c(-1,0,fleets))
  new_df <- rbind(new_df, expand.grid(Yr=years, Seas=1, Sex=1, BioPattern = 1, BirthSeas =1, Fleet = -2))
  names(new_df)<- c("Yr","Seas","Sex","Bio_Pattern","BirthSeas","Fleet")

  agecols <- seq(0,maximum_age)

  new_df[,as.character(agecols)] <- NA

  #Create list
  wt_at_age_list <- list()

  modify_matrices <- function(matr_to_turn){
    matr_to_turn$kg <- matr_to_turn$atoutput/1000
    return_mat <-  dcast(data = matr_to_turn,
                                         formula = year ~agecl,
                                         value.var = "kg")

    return(return_mat)
  }

  catch_mean_wt_matrix$kg <- catch_mean_wt_matrix$atoutput/1000
  surv_mean_wt_matrix$kg <- surv_mean_wt_matrix$atoutput/1000

  catch_meanwt <- modify_matrices(catch_mean_wt_matrix)

  surv_meanwt <- modify_matrices(catch_mean_wt_matrix)


  browser()

  if(sum(catch_meanwt[,names(catch_meanwt)!="time"])!=length(agecols)){
    agecol_ind <- which((agecols %in% names(catch_meanwt)[!(names(catch_meanwt) %in% c("time","year"))]))
  }

  #Add in weight at age for fishing
  new_df[new_df$Fleet==1,(7+agecols)[agecol_ind]] <- catch_meanwt[,-c("year")]


  if(sum(surv_meanwt[,names(surv_meanwt)!="time"])!=length(agecols)){
    surv_agecol_ind <- which((agecols %in% names(surv_meanwt)[!(names(surv_meanwt) %in% c("time","year"))]))
  }
  #Add in weight at age for survey
  new_df[new_df$Fleet==2,(7+agecols)[surv_agecol_ind]] <- surv_meanwt[,-c("year")]

  #Add in mid year pop weight at age
  new_df[new_df$Fleet==-1,(7+agecols)[surv_agecol_ind]] <- surv_meanwt[,-c("year")]

  #Add in beg year pop weight at age
  new_df[new_df$Fleet==0,(7+agecols)[surv_agecol_ind]] <- surv_meanwt[,-c("year")]

  #Add in mat_fecundity
  new_df[new_df$Fleet==-2,(7+agecols)[surv_agecol_ind]] <- surv_meanwt[,-c("year")]*mat_vector


  new_df <- rbind(new_df,c("-9999",rep(1,5+length(seq(0,maximum_age)))))
  written_file<- r4ss::SS_writewtatage(wt_at_age_list, overwrite = TRUE)


  return(written_file)
}
