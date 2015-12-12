#' Calculate total mortality for age structured groups
#'
#' This function uses the YOY.txt and Nums to calculate Z.
#' @param YOY File name of the YOY.txt file
#' @param Nums Object containing the number at stage.
#' @params species_info Species Code and Name.
#' @family calc functions
#' @return A data table with the time varying Z.
#' @author Sean Lucey
#' @import data.table
#' @export
calc_Z <- function(YOY, Nums, species_info){
  species.code <- species_info[1]
  #Read in recruits from YOY.txt
  recruits <- as.data.table(YOY)
  recruits <- recruits[, list(Time, get(paste(species.code, '.0', sep = '')))]
  setnames(recruits, 'V2', 'recruits')
  setnames(recruits, 'Time', 'time')
  #mg C converted to wet weight in tonnes
  recruits[, recruits := recruits / 0.00000002]
  #Divide by Redfield ratio
  recruits[, recruits := recruits / 5.7]
  #Convert days to years to match
  #Need to check if all models will be in this time step
  recruits[, time := time / 365]

  #Sum over all boxes/depth/cohorts
  Nums <- as.data.table(Nums)
  totnums <- Nums[, sum(atoutput), by = time]
  setnames(totnums, c('time', 'V1'), c('time', 'atoutput'))

  #Combine recruits and numbers
  totnums <- merge(totnums, recruits, by = 'time')

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
