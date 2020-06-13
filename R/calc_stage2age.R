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
#' bps=load_bps(dir = dir, "functionalGroups.csv", file_init)
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
#calc_stage2age <- function(dir, nums_data, biolprm, YOY, fgs) {
# dir not used in the function
# need runprm to get correct toutinc to pass to calc_Z
calc_stage2age <- function(nums_data, biolprm, yoy, fgs, runprm) {

  # subset the yoy for those species that are included in the fgs file
  # that are turned on
  species.code <- fgs$Code
  turnedon <- fgs[fgs$IsTurnedOn > 0, ]
  names <- turnedon$Code

  # Figure out the groups that have multiple ages classes in each stage (or
  # cohort)
  multiple_ages <- turnedon[turnedon$NumAgeClassSize>1, c(1,4,10)]
  num_multi_age <- dim(multiple_ages)[1]

  ntimesteps <- length(unique(nums_data$time))

  # For each species with multiple ages, use the calc_Z function to get time
  # varying Z values and put that all into one dataframe
  Z.dataframe <- data.frame()
  #for(i in 1:num_multi_age) {
    #temp_nums <- nums_data[nums_data$species==multiple_ages$Name[i],]
    temp_nums <- nums_data[nums_data$species %in% multiple_ages$Name,]
    temp_Z <- calc_Z(yoy=yoy, nums=temp_nums, fgs=fgs, biolprm=biolprm, toutinc=runprm$toutinc)
    #Z.dataframe <- rbind(Z.dataframe, temp_Z)
    Z.dataframe <- temp_Z
  #}

  # HACK for now, since some Z values are negative, I am replacing them
  # with a random number
  randomZ <- runif(length(which(Z.dataframe$atoutput<0)))
  Z.dataframe[which(Z.dataframe$atoutput<0),c("atoutput")] <- randomZ

  # rename the "atoutput" column, since it is really just the mortality, and
  # we want to retain it for the next step of merging
  colnames(Z.dataframe)[6] <- c("Z")

  # merge together nums_data and the output Z.dataframe:
  new_nums <- merge(nums_data, Z.dataframe[,c("species", "time", "Z")],
                    by=c("species", "time"), all.x=TRUE)


  # since we have to expand the number of rows for groups with multiple true
  # age classes, we will loop through species and time, creating a new list
  # that each element will be a single species in a single time step but
  # across ages and boxes (these wil all be put together in the end)
  temp.list <- list()

  #SKG subset turnedon and only loop through species present in the nums input
  turnedon_sub <- turnedon %>%
    filter(Name %in% new_nums$species)

  names <- turnedon_sub$Code

  for(i in 1:length(names)) {
    # looping all species -- those with or without multiple true ages

    group.i <- turnedon_sub$Name[i]
    nums_species <- new_nums[new_nums$species==group.i,] # might need to
    num_ages <- turnedon_sub$NumAgeClassSize[i]
    sp_times <- sort(unique(nums_species$time))
    n_sp_tsteps <- length(sp_times)
    # these last pieces are needed because not all species are present at all times

    # Check if multiple true ages, if not then just save species_nums to list
    if(num_ages==1) { temp.list[[length(temp.list)+1]] <- nums_species
    } else if(num_ages>1) {
      # take out the part of the Z.dataframe for species group i
      Zvals <- Z.dataframe[Z.dataframe$species==group.i,]

      # create empty list for this species i, each element in the list will be
      # a different time step
      list_species <- list()

      for(j in sp_times) { # looping through time
        # get the Z val for the timestep in question
        Zval.j <- Zvals$Z[Zvals$time==j]

        # and turn the Z value into a vector of survival values across the
        # number of true ages for each age class for this species
        nums_vec <- 1
        for(k in 1:(num_ages-1)) {
          nums_vec <- c(nums_vec, exp(-Zval.j*k))
        }
        nums_proportion <- nums_vec/sum(nums_vec)

        # take out the species numbers only for time step j
        nums_sp_subset <- nums_species[nums_species$time==j, ]

        # create an empty list, each element of this list will be a row from
        # the nums_sp_subset data frame that is split into multiple pieces
        # to make the atoutput column have the correct dimensions
        list_ages <- list()

        # loop through all the rows in nums_sp_subset
        for(l in 1:nrow(nums_sp_subset)) {
          nums_row <- nums_sp_subset[l,]
          # create new data frame with same columns, but just more rows to
          # account for all the true ages
          new_rows <- data.frame(species=nums_row$species,
                                 time=nums_row$time,
                                 agecl=seq((nums_row$agecl*num_ages - num_ages+1),
                                           nums_row$agecl*num_ages),
                                 polygon=nums_row$polygon,
                                 layer=nums_row$layer,
                                 atoutput=nums_row$atoutput*nums_proportion,
                                 Z=nums_row$Z
                                 )

          ### NOTE: the agecl piece is funny -- because now we are making many
          # more age classes for some... I think this works? But might want
          # another set of eyes to check my logic

          list_ages[[length(list_ages)+1]] <- new_rows
        }

        # now combine all the elements of the list for different rows and make
        # it an element of the list_species (to represent one time step)
        list_species[[length(list_species)+1]] <- do.call("rbind", list_ages)
      }
      temp.list[[length(temp.list)+1]] <- do.call("rbind", list_species)
    }
  }

  # Now just create the output
  finalout_withZ <- do.call("rbind", temp.list)

  ### CHECK:
  # do we want to remove the column of Z values? I think so
  finalout <- finalout_withZ[,-7]


  return(finalout)

}

