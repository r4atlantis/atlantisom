#'Function to write an empirical weight at age SS file (wt_at_age.ss) from Atlantis output
#'@description
#'@param age_classes the vector of age classes in the model
#'@param years vector of years in the model
#'@param fleets vector of fleets
#'@param seas vector of seasons
#'@param catch_mean_wt_matrix mu_weight object in grams
#'@param surv_mean_wt_matrix mu_weight object in grams
#'@param fleet_month vector of months (1-12) when sampling occurs
#'@param mat_vector vector of maturity ogive, must be the same length as \code{age_classes}
#'@author Christine Stawitz

SS_write_EWAA <- function(age_classes, years, fleets, seas, catch_mean_wt_matrix, surv_mean_wt_matrix, fleet_month, mat_vector,
                          recruit_size){

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
    matr_to_turn <- filter(matr_to_turn, year %in% years)
    return_mat <-  dcast(data = matr_to_turn,
                                         formula = year ~agecl,
                                         value.var = "kg",
                         fun.aggregate = mean)

    return(return_mat)
  }

  catch_mean_wt_matrix$kg <- catch_mean_wt_matrix$atoutput/1000
  surv_mean_wt_matrix$kg <- surv_mean_wt_matrix$atoutput/1000

  catch_meanwt <- modify_matrices(catch_mean_wt_matrix)

  surv_meanwt <- modify_matrices(surv_mean_wt_matrix)



  if(sum(catch_meanwt[,!(names(catch_meanwt) %in% c("year","time"))])!=length(agecols)){
    agecol_ind <- which((agecols %in% names(catch_meanwt)[!(names(catch_meanwt) %in% c("time","year"))]))
  }

  #Add in weight at age for fishing
  new_df[new_df$Fleet==1,(7+agecols)[agecol_ind]] <- catch_meanwt[,-c("year")]


  if(sum(surv_meanwt[,!names(surv_meanwt) %in% c("year","time")])!=length(agecols)){
    surv_agecol_ind <- which((agecols %in% names(surv_meanwt)[!(names(surv_meanwt) %in% c("time","year"))]))
  }
  #Add in weight at age for survey
  new_df[new_df$Fleet==2,(7+agecols)[surv_agecol_ind]] <- surv_meanwt[,-c("year")]

  #Add in mid year pop weight at age
  new_df[new_df$Fleet==-1,(7+agecols)[surv_agecol_ind]] <- surv_meanwt[,-c("year")]

  #Add in beg year pop weight at age
  new_df[new_df$Fleet==0,(7+agecols)[surv_agecol_ind]] <- surv_meanwt[,-c("year")]

  #Add in mat_fecundity
  new_df[new_df$Fleet==-2,(7+agecols)[surv_agecol_ind]] <- sweep(surv_meanwt[,-c("year")],2,mat_vector,"*")

  #If we don't have any recruits, set size of recruits to reasonable default
  which_empty_recs <- which(is.na(new_df[,"0"]))
  if(length(which_empty_recs)>0){
    new_df[which_empty_recs,"0"] <- rep(recruit_size, length(which_empty_recs))
  }

  #Function to linearly interpolate
  interp_waa <- function(waa_row){
    na_values <- which(is.na(waa_row))

    #Check if any NA values in that row
    if(length(na_values)>0){
      waa_row[na_values] <- (waa_row[na_values-1]+waa_row[na_values+1])/2
    }
    return(waa_row)
  }

  for(i in seq(nrow(new_df))){
    new_df[i,] <- interp_waa(new_df[i,])
  }

  written_file<- SS_writewtatage(new_df, overwrite = TRUE)

  return(written_file)
}
