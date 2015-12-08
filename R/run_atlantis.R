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
#'  \item{"[...]TOTCATCH.nc"}
#'  \item{"[...]DietCheck.txt"}
#' },
#' where [...] specifies the entry used for the \code{scenario} argument.
#'
#' @family run functions
#' @author Sean Lucey
#'
#' @param scenario output name of the Atlantis scenario.
#' @template dir
#'
#' @return Returns a list object.
#' @export
run_atlantis <- function(scenario, dir = getwd(),
  file_fgs, ){

  # Create file names
  if (is.null(dir)) {
    file.fgs <- file_fgs
  } else {
    file.fgs <- file.path(dir, file_fgs)
  }

  # Read in information
  # Read in the functional groups csv since that is used by many functions
  fgs <- read_functionalgroups(file.fgs)

  TOTCATCH <- file.path(dir, paste(scenario, '_TOTCATCH.nc'))
  DietCheck <- file.path(dir, paste(scenario, 'DietCheck.txt'))

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



