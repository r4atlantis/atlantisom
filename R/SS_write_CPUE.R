#'Function to write the CPUE from an atlantis OM into a
#'Stock Synthesis 3.3 estimation model
#'@param ss_data_list the list of the SS .dat file that #'comes from \code{SS_readdat}
#'@param cpue_data a list, where each list item is a #'vector of the observations for each index
#'@param CVs a vector of numbers representing the CV around the index
#'@param data_years a vector, where each item is the length of the index
#'@param sampling_month a list, where each item is a vector representig the month of sampling for each index
#'@param  units must be one of "numbers" or "biomass"
SS_write_CPUE <- function(ss_data_list, cpue_data,
                 CVs, data_years,
                 sampling_month, units){

  ss_data_list$CPUE <- ss_data_list$CPUE[0,]
  k <- 1
  start_year <- ss_data_list$styr
  for(i in 1:length(cpue_data)){

    if(units == "numbers") {
      cpue_data[[i]] <- round(cpue_data[[i]]/1000,0)
    } else{
      cpue_data[[i]] <- round(cpue_data[[i]],0)
      }

    indices <- (k:(k+data_years[i]-1))
  ss_data_list$CPUE[indices,"year"] <- start_year:(start_year+data_years[i]-1)
  ss_data_list$CPUE[indices, "obs"] <- cpue_data[[i]]
  ss_data_list$CPUE[indices, "seas"] <- sampling_month[[i]]
  ss_data_list$CPUE[indices, "se_log"] <- rep(CVs[i], length(indices))
  ss_data_list$CPUE[indices, "index"] <- rep(i, length(indices))

  k <- indices[length(indices)]+1

  }

  return(ss_data_list)
}
