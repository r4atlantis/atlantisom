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
#' YOY <- load_yoy(dir, file_yoytxt="outputSETASYOY.txt")

## ACTUAL FUNCTION ##
calc_stage2age <- function(dir, nums_data, biolprm, YOY) {
  
  ## Add:
  # First need to add a check for which stages are turned on and only take
  # those from the bioprm file
  
  # Figure out the groups that have multiple ages classes in each stage (or
  # cohort), will end up only looping over these groups
  multiple_ages <- fgs[fgs$NumAgeClassSize>1, c(1,4,10)]
  num_multi_age <- dim(multiple_ages)[1]

  ntimesteps <- length(unique(nums_data$time))

  

  # loop through species that have multiple ages in a cohort, to calculate
  # their Z values over time
  
  ## Add: 
  # create some empty data frame that will add each output from calc_Z to
  # in the for loop
  Z.dataframe <- ()
  for(i in 1:num_multi_age) {
    temp_nums <- nums_data[nums_data$species==multiple_ages$Name[i],]
    temp_Z <- calc_Z(YOY=YOY, Nums=temp_nums,
                     species_info=multiple_ages[i,1:2])
    num_ages <- multiple_ages$NumAgeClassSize[i]



    # need to consider each time step -- that is where I am leaving it, but
    # for a time step, something like the following needs to occur:
    for(j in 1:(ntimesteps-1)) { # won't work to skip 0 but I have to think a bit
    ## Add:
    # to the data frame that was created above, tack this new temp_Z onto the end
    Z.dataframe[i] <- temp_Z # this is not right, but is a place holder
  }
  
  new_nums <- () # merge together nums_data and the output Z.dataframe from the
  # calc_Z function loop
  
  empty <- list() # create list that is empty and we will add to
  # loop through species and time
  for(i in 1:dim(new_nums)[1]) { # looping species -- both those that have multiple
    # true ages and those that do not
    temp_nums <- new_nums[new_nums$species==biolprm$Name[i],] # might need to 
    # change the 'biolprm' to be whatever is the list of groups that were turned on
    
    for(j in 1:ntimesteps) { # now loop through all the time steps
      
      if() # if statement to check if the species in selected in this loop has
        # multiple true ages
        # if it does NOT have multiple ages
        empty[[length(empty)+1]] <- temp_nums
      
      else () {
      # if the species DOES have multipel true ages
      
      # to determine the number of ages for each group that has multipel true ages:  
      num_ages <- multiple_ages$NumAgeClassSize[i]
      Zval <- temp_Z$Z[j]
      
      # horribly named, but below is a subet of the species specific data frame 
      # and taking out only the time of interest
      nums_time_sub <- temp_nums[temp_nums$time==j, ]
      nums_vec <- 1
      for(k in 1:(num_ages-1)) {
        nums_vec <- c(nums_vec, exp(-Zval*k))
      }
      nums_proportion <- nums_vec/sum(nums_vec)
      # stopping in here -- there needs to be some interesting multiplications
      # to make all 'atoutput' multiply by the nums_proportion to break it into pieces

      # not sure the best way to do that.
      nums_vec <- rep(nums_proportion, dim(nums_time_sub)[1]*2)
      # this probably needs to change to not be 10, since in the new code some
      # species can have less than 10 cohorts? Butthis is it for now


      # maybe a better way than so many for loops can be figured out?




  # then outside the for loop, combine the specific age data to the
  # species with only stage data

  # one possible issue: number of columns might be larger than the other data
  # frames


}
