#' Approximating the true numbers at age within each box, functional group and
#' time step.
#'
#' Will be called in the run_truth file, and requires inputs from
#' \code{\link{load_nc}} for the "Nums" variable, and the
#' \code{\link{load_biolprm}}.
#'
#' @family calc functions
#' @author Emma E Hodgson and Kelli Faye Johnson
#'
#' @template dir
#' @return A \code{data.frame} in long format with the following coumn names:
#'   Species, timestep, polygon, agecl, and atoutput (i.e., variable). This is 
#'   the same as the input Nums data frame however, agecl will be TRUE numbers
#'   by age class -- just named the same as the input to ensure make it work 
#'   with the other functions
#'   
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
#' #when calc_stage2age is run in the run_truth, it will need to have the nums
#' #data frame and the bioprm already read in:
#' nums_data <- load_nc(dir = dir,
#'                      file_nc="outputSETAS.nc",
#'                      bps=bps, fgs=fgs, select_groups=select_groups,
#'                      select_variable = "Nums",
#'                      check_acronyms = TRUE, bboxes = bboxes)
#' biolprm <- load_biolprm(dir, file_biolprm="VMPA_setas_biol_fishing_Trunk.prm")
#' YOY <- load_yoy(dir, file_yoy="outputSETASYOY.txt")

## ACTUAL FUNCTION ##
calc_stage2age <- function(dir, nums_data, biolprm, YOY, fgs) {

  # subset the yoy for those species that are included in the fgs file
  # that are turned on
  species.code <- fgs$Code
  turnedon <- fgs[fgs$IsTurnedOn > 0, ]
  names <- turnedon$Code
  
  # Figure out the groups that have multiple ages classes in each stage (or
  # cohort), will end up only looping over these groups
  multiple_ages <- turnedon[turnedon$NumAgeClassSize>1, c(1,4,10)]
  num_multi_age <- dim(multiple_ages)[1]

  ntimesteps <- length(unique(nums_data$time))  
  
  # For each species with multiple ages, use the calc_Z function to get time
  # varying Z values and put that all into one dataframe
  Z.dataframe <- data.frame()
  for(i in 1:num_multi_age) {
    temp_nums <- nums_data[nums_data$species==multiple_ages$Name[i],]
    temp_Z <- calc_Z(YOY=YOY, Nums=temp_nums,
                     species_info=multiple_ages[i,1:2])
    Z.dataframe <- rbind(Z.dataframe, temp_Z)
  }
  
  #### Check ####
  # The following merge() may or may not work -- test when the Z loop works
  
  # merge together nums_data and the output Z.dataframe from the
  new_nums <- merge(nums_data, Z.dataframe, by="species")
  
  
  # since we have to expand the number of rows for groups with multiple true
  # age classes, we will loop through species and time, creating a new list
  # that each element will be a single species in a single time step but 
  # across ages and boxes (these wil all be put together in the end)
  temp.list <- list() 
  
#   for(i in 1:dim(new_nums)[1]) { 
#     # looping species -- both those that have multiple true ages and those that 
#     # do not
#     group.i <- turnedon$Name[i]
#     temp_nums <- new_nums[new_nums$species==group.i,] # might need to 
#     num_ages <- turnedon$NumAgeClassSize[i]
#     
#     # Only need to loop through time for species that do have multiple true 
#     # ages, so check that. If they do not, then just add the temp_nums to 
#     # the list for that group.i
#     if(num_ages==1) { temp.list[[length(temp.list)+1]] <- temp_nums
#     } else (if num_ages>1) {
#       # take out the part of the Z.dataframe for the group in this i
#       Zvals <- Z.dataframe[Z.dataframe$species==group.i,]
#       for(j in 1:ntimesteps) { # now loop through all the time steps for those
#         # species with multiple age classes in a stage
#         
#         # get the Z val for the timestep in question
#         Zval.j <- Zvals$time[Zvals$time==j]
#         
#         # take out the species numbers only for time step j
#         nums_subset <- temp_nums[temp_nums$time==j, ]
#         nums_vec <- 1
#         for(k in 1:(num_ages-1)) {
#           nums_vec <- c(nums_vec, exp(-Zval.j*k))
#         }
#         nums_proportion <- nums_vec/sum(nums_vec)
#         
#         ##### STUCK #####
#         # stopping in here -- there needs to be some interesting multiplications
#         # to make all 'atoutput' multiply by the nums_proportion to break it into 
#         # pieces and to make those pieces be different rows in the data.frame
#         # essentially all 'atoutput' elements for the species within this loop
#         # need to be multiplied by the nums_proportions and each of those need
#         # to be a row in the data frame... all other column entries will remain
#         # the same, except for the the agecl column that needs to be subdivided
#         
#         out.nums <- ()
#         
#         temp.list[[length(temp.list)+1]] <- out.nums
#       }
#     }
#   }
  
  # then there needs to be some way to combine the list pieces so that 
  #output <- () # combine the list into one object
  
  
  return(output)
  
}

