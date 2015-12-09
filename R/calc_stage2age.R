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
#' @param dir
#' @param nums_data
#' @param biolprm # I will explain and change these as I work on this function
#' @return A \code{data.frame} in long format with the following coumn names:
#'   Species, timestep, polygon, TRUEagecl, and atoutput (i.e., variable).
#' @export
#'

# This is just to bring in an example data frame I can use to write the code,
# much of the beginning code will be removed as I actually write the function, 
# but I wanted to push what I have so far!
dir="/Users/ehodgson/Dropbox/UW/files/EH10212015_pH2013_AllOn_mort0.1"
file_nc="outputCCV3.nc"
fgs=read_functionalgroups("CalCurrentV3Groups.csv")
file_init="DIVCalCurrentV3_Biol.nc"
bps=load_bps(dir = getwd(), fgs, file_init)
select_groups=load_groups("CalCurrentV3Groups.csv")
select_variable="Nums"
box.info=load_box(file_bgm="CalCurrentV3_utm.bgm")
bboxes=get_boundary(box.info)


# when calc_stage2age is run in the run_atlantis, it will need to have the nums
# data frame and the bioprm already read in:
nums_data <- load_nc(dir = getwd(), 
                     file_nc="outputCCV3.nc", 
                     bps=bps, fgs=fgs, select_groups=select_groups,
                     select_variable = "Nums",
                     check_acronyms = TRUE, bboxes = bboxes)

biolprm <- load_biolprm(dir, file_biolprm="CalCurrentV3_Biol.prm")

## ACTUAL FUNCTION ##
calc_stage2age <- function(dir, nums_data, biolprm) {

  # Figure out the groups that have multiple ages classes in each stage (or 
  # cohort), will end up only looping over these groups
  multiple_ages <- fgs$Name[fgs$NumAgeClassSize>1]
  num_multi_age <- length(multiple_ages)
  
  # vector of boxes in the model, since we need to loop over boxes
  num_boxes = unique(nums.data$polygon)
  
  # determining the timestep output
  file.prm <- dir(dir, pattern = "run\\.prm", full.names = TRUE)
  # have to add in load_meta -- since I will need to figure out the time step
  # from that
  
  # Here there will be some sort of if statement -- we need the output to be 365
  # days in order to ensure that recruitment is being captured at the same time 
  # each year... I am not sure what to do since we need quarterly output,
  # maybe it will not matter? But I am concerned about 91 day out put since
  # we get year outputs on 364 days, which may change the measurement each year?
  
  for(k in 1:num_multi_age) {
    for(i in 1:num_boxes) {
      for(j in 2:num_timestep) { # start at the second since we subtract the first
        # in this loop we will calculate for each species and cohort in each box, 
        # the change in numbers between years (cohort 2 - cohort1)
        
        # the difference will then be divided by the stage class size for the younger
        # stage class (cohort 1)
        
        # we hope these will be positive -- if it is negative then we have issues
        
        
        # Then there will be an if statement to check if it is negative, and if it
        # in then we will go back in stored values to find the last positive value
        # and use that for mortality
        
        # And using the mortality value there is then some multiplication that 
        # happens to get the final values???
        # I am brain fried and am not sure about that
        
      }}}
  
}