#' Calculate total mortality for age structured groups
#'
#' This function uses the YOY.txt and Nums to calculate Z.
#' @params YOY File name of the YOY.txt file
#' @params Nums Object containing the number at stage.
#' @params species.code Species specific code to extract the data from YOY.
#' @family calc functions
#' @return A data table with the time varying Z.
#' @author Sean Lucey
#' @import data.table
#' @export
calc_Z <- function(YOY, Nums, species.code){
  #Read in recruits from YOY.txt
  recruits <- as.data.table(read.table(YOY, header = T))
  recruits <- recruits[, list(Time, get(paste(species.code, '.0', sep = '')))]
  setnames(recruits, 'V2', 'recruits')
  #mg C converted to wet weight in tonnes
  recruits[, recruits := recruits / 0.00000002]
  #Divide by Redfield ratio
  recruits[, recruits := recruits / 5.7]
  #Convert days to years to match
  #Need to check if all models will be in this time step
  recruits[, Time := Time / 365]
  
  #Sum over all boxes/depth/cohorts
  Nums <- as.data.table(Nums)
  totnums <- Nums[, sum(atoutput), by = time]
  setnames(totnums, c('time', 'V1'), c('Time', 'atoutput'))
  
  #Combine recruits and numbers
  totnums <- merge(totnums, recruits, by = 'Time')
  
  #Calculate survivors
  totnums[, survive := (atoutput - recruits) / shift(atoutput)]
  
  # Find the first positive value in the column of 'survive'
  pos_survive <- totnums$survive[totnums$survive>0][2] # the first will always
  # be NA so just take the second
  final_survival <- pos_survive
  
  # make sure that all values for 'final_survival' are positive, otherwise
  # there is an error in the output since the 'survival' is logged to convert
  # to Z.
  for(i in 2:dim(totnums)[1]) {
    if(totnums$survive[i] > 0) {
      pos_survive <- totnums$survive[i]}
    final_survival <- c(final_survival, pos_survive)
  }
  
  totnums <- cbind(totnums, final_survival)
  
  #Calculate Z
  totnums[, Z := -1 * log(final_survival)]  
  return(totnums[, list(Time, Z)])
}
