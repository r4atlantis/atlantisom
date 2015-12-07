#' Load Atlantis scenario output
#'
#' Reads in data generated from an Atlantis scenario and returns a list
#' containing the desired information. The list contains the 'truth' as known
#' from the Atlantis scenario. The truth can later be sampled
#' from to create a data set with observation error.
#' Currently, the \code{run_atlantis} depends on the following files
#' being in your working directory:
#' \itemize{
#'  \item{"functionalGroups.csv"}
#'  \item{"...TOTCATCH.nc"}
#'  \item{"...DietCheck.txt"}
#' }
#'
#' @family run functions
#' @author Sean Lucey
#'
#' @param scenario output name of the Atlantis scenario.
#' @param path location of the dietfile.
#'
#' @return Returns a list object.
#' @export
run_atlantis <- function(scenario, path = "."){
  #Create file names for NetCDF files and DietCheck.txt
  groups <- file.path(path, 'functionalGroups.csv')
  TOTCATCH <- file.path(path, paste(scenario, '_TOTCATCH.nc'))
  DietCheck <- file.path(path, paste(scenario, 'DietCheck.txt'))

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



