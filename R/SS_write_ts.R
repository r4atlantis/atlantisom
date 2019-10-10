#'Function to write the CPUE from an atlantis OM into a
#'Stock Synthesis 3.3 estimation model
#'@export
#'@param ss_data_list the list of the SS .dat file that #'comes from \code{SS_readdat}
#'@param cpue_data a list, where each list item is a #'vector of the observations for each index
#'@param CVs a vector of numbers representing the CV around the index
#'@param data_years a vector, where each item is the length of the index
#'@param sampling_month a list, where each item is a vector representig the month of sampling for each index
#'@param  units a vector where each entry must be one of "numbers" or "biomass"
#'@param fleets a list of vectors, in each vector  each entry is the fleet number
#'@param data_type a vector with length = \code{length(ts_data)}, each entry must be either "CPUE" or "catch"
SS_write_ts <- function(ss_data_list, ts_data,
                 CVs, data_years,
                 sampling_month, units,
                 fleets,
                 data_type){


  names_cpue <- c("year","seas","index", "obs", "se_log")
  names_catch <- c("year","seas","fleet", "catch", "catch_se")

  #Clear existing data
  if("CPUE" %in% data_type){
  ss_data_list$CPUE <- data.frame(matrix(ncol=5, nrow=0))
  colnames(ss_data_list$CPUE) <- names_cpue
  }
  if("catch" %in% data_type){
  ss_data_list$catch <- data.frame(matrix(ncol=5, nrow=0))
  colnames(ss_data_list$catch) <- names_catch
  }


  k_CPUE <- k_catch <- 1
  start_year <- ss_data_list$styr
  for(i in 1:length(ts_data)){
    k <- switch(data_type[i],"CPUE"=k_CPUE,
                "catch"=k_catch)

    col_names <- switch(data_type[i],
                        "CPUE"=names_cpue,
                        "catch"=names_catch)

    if(units[i] == "numbers") {
      ts_data[[i]] <- round(ts_data[[i]]/1000,0)
      ss_data_list$units_of_catch[i] <- 2
      ss_data_list$fleetinfo$units[i] <- 2
      ss_data_list$CPUEinfo$Units[i] <-2
    } else{
      ts_data[[i]] <- round(ts_data[[i]],0)
      ss_data_list$units_of_catch[i] <- 1
      ss_data_list$fleetinfo$units[i] <- 1
      ss_data_list$CPUEinfo$Units[i] <-1
      }

    indices <- (k:(k+length(data_years[[i]])-1))
  ss_data_list[[data_type[i]]][indices,"year"] <- data_years[[i]]
  ss_data_list[[data_type[i]]][indices, "seas"] <- sampling_month[[i]]

  ss_data_list[[data_type[i]]][indices, col_names[4]] <- ts_data[[i]]
  ss_data_list[[data_type[i]]][indices, col_names[5]] <- rep(CVs[i], length(indices))
  ss_data_list[[data_type[i]]][indices, col_names[3]] <- rep(fleets[i], length(indices))

  if(data_type[i]=="CPUE"){
    k_CPUE <- k_CPUE+length(data_years[[i]])
  } else{

    k_catch <- k_catch+length(data_years[[i]])
  }
  }

  return(ss_data_list)
}
