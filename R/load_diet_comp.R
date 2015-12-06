#'Load Atlantis Diet Composition
#'
#'Uses the diet_check.txt output from Atlantis to be used in the Atlantis
#'Operating model.
#'
#'@family AtlantisOM
#'
#'@param dietfile file name of the diet_check.txt output from Atlantis.
#'
#'@return Returns a data frame of the data to be exported to the AtlantisOM list
#'  object.
#'@import data.table
#'@export
load_diet_comp <- function(dietfile){
  diet <- as.data.table(read.table(dietfile), header = TRUE)

  #remove unnessesary columns and add ones that aren't present in the data
  diet[, Stock   := NULL]
  diet[, Polygon := NA]
  diet[, Layer   := NA]

  #Change column order
  columns <- names(diet)[which(!names(diet) %in% c('Time', 'Group', 'Cohort',
                                                   'Polygon', 'Layer'))]
  setcolorder(diet, c('Group', 'Cohort', 'Polygon', 'Layer', 'Time', columns))
  setnames(diet, c('Group', 'Cohort'), c('Species', 'agecl'))
  diet <- as.data.frame(diet)
  return(diet)
}
