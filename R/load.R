load_diet <- function(dietfile, path){
  diet <- as.data.table(read.table(paste(path, dietfile, sep = ""), header = TRUE))
  
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
