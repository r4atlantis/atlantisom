#'Function to write an empirical weight at age SS file (wt_at_age.ss) from Atlantis output
#'@description
#'@param age_classes the vector of age classes in the model
#'@param mean_wt_matrix mu_weight object
#'@param weight_age_list list in the format of weight at age input to SS
#'@author Christine Stawitz

SS_write_EWAA <- function(age_classes, years, fleets, mean_wt_matrix, weight_age_list){

  #First value in empirical weight at age file is the maximum age; use this
  maximum_age <- max(age_classes)

  #Populate empty data frame
  new_df <- expand.grid(Yr=years, Seas=c(1,2), Sex = 1, BioPattern = 1, BirthSeas = 1,Fleet=c(-1,0,fleets))
  new_df <- rbind(new_df, expand.grid(Yr=years, Seas=1, Sex=1, BioPattern = 1, BirthSeas =1, Fleet = -2))
  names(new_df)<- c("Yr","Seas","Sex","Bio_Pattern","BirthSeas","Fleet")

  agecols <- seq(0,maximum_age)

  new_df <- rbind(new_df,c("-9999",rep(1,5+length(seq(0,maximum_age)))))
  written_file<- r4ss::SS_writewtatage(wt_at_age_list, overwrite = TRUE)


  return(written_file)
}
