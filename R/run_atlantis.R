#'Create Atlantis data set
#'
#'Uses the diet_check.txt output from Atlantis to be used in the Atlantis
#'Operating model.
#'
#'@family AtlantisOM
#'
#'@param scenario output name of the Atlantis scenario.
#'@param path location of the dietfile.
#'
#'@return Returns an AtlantisOM list object.
#'@import data.table
#'@export
run_atlantis <- function(scenario, path = "."){
  #Create file names for NetCDF files and DietCheck.txt
  groups <- file.path(path, 'functionalGroups.csv')
  TOTCATCH <- file.path(path, paste0(scenario, '_TOTCATCH.nc'))
  DietCheck <- file.path(path, paste0(scenario, 'DietCheck.txt'))

  #Extract from NetCDF files
  numcatch <- load_atlantis_ncdf(TOTCATCH, groups, select_groups,
                                 'Nums', remove_bboxes = T,
                                 check_acronyms = T)

  #Extract Diet Matrix
  diet_comp <- load_diet_comp(DietCheck)

  #Create list object
  Atlantis <- list(numcatch, diet_comp)
  return(Atlantis)
}



