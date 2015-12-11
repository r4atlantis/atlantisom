#' Approximating the true numbers at age within each box, functional group and
#' time step.
#'
#' Will be called in the run_atlantis file, and requires inputs from
#' \code{\link{load_nc}} for the "Nums" variable, and the \code{\link{load_meta}}
#' and finally the \code{\link{load_biolprm}}.
#'
#' @family calc functions
#' @author Emma Hodgson
#'
#' @template dir
#' @return A \code{data.frame} in long format with the following coumn names:
#'   Species, timestep, polygon, TRUEagecl, and atoutput (i.e., variable).
#' @export
#'
#' @examples
#' #This is just to bring in an example data frame I can use to write the code,
#' #much of the beginning code will be removed as I actually write the function,
#' #but I wanted to push what I have so far!
#' dir <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file_nc="outputSETAS.nc"
#' fgs=load_fgs(dir = dir, "functionalGroups.csv")
#' file_init="INIT_VMPA_Jan2015.nc"
#' bps=load_bps(dir = dir, fgs, file_init)
#' select_groups=fgs$Name[fgs$IsTurnedOn > 0]
#' select_variable="Nums"
#' box.info=load_box(dir = dir, file_bgm="VMPA_setas.bgm")
#' bboxes=get_boundary(box.info)
#' #when calc_stage2age is run in the run_atlantis, it will need to have the nums
#' #data frame and the bioprm already read in:
#' nums_data <- load_nc(dir = dir,
#'                      file_nc="outputSETAS.nc",
#'                      bps=bps, fgs=fgs, select_groups=select_groups,
#'                      select_variable = "Nums",
#'                      check_acronyms = TRUE, bboxes = bboxes)
#' biolprm <- load_biolprm(dir, file_biolprm="VMPA_setas_biol_fishing_Trunk.prm")
#' recruits <- as.data.table(read.table(YOY, header = T))

## ACTUAL FUNCTION ##
calc_stage2age <- function(dir, nums_data, biolprm, YOY_name) {
  # Figure out the groups that have multiple ages classes in each stage (or
  # cohort), will end up only looping over these groups
  multiple_ages <- fgs[fgs$NumAgeClassSize>1, c(1,4)]
  num_multi_age <- dim(multiple_ages)[1]
  
  temp.Zs <- matrix(nrow=40, ncol=99)
  for(i in 1:num_multi_age) {
    temp_nums <- nums_data[nums_data$species==multiple_ages$Name[i],]
    temp_Z <- calc_Z(YOY="outputCCV3YOY.txt", Nums=temp_nums, 
                     species.code=multiple_ages$Code[i])
    temp.Zs[i,] <- temp_Z$Z[-1]
    
    
    # now inside of this loop get the calculate Z for each species in the 
    # multipl_ages list
    
    # then apply that Z to determine the proportions at age 
    # and for each layer time step and age class, then multiply by the numbers 
    # at age. 
    
  }
  
  write.csv(temp.Zs, "~/Desktop/temp_file.csv")
  
  
  
  # then outside the for loop, combine the specific age data to the 
  # species with only stage data
  
  # one possible issue: number of columns might be larger than the other data
  # frames
  

}
