#'Function to write age and length composition data from \code{atlantisom} to Stock Synthesis 3.30.
#'@param ss_data_list the list object containing SS data
#'@param comp_matrix the matrix of composition data (age or length)
#'@param data_rows a list, where each entry is a vector corresponding to the year column of the composition data. For length composition data, this is the 1:nyears repped each row for the number of length bins. For age composition data, this is nyears
#'@param sampling_month a list where each list item is a vector of the sampling month for each composition data type
#'@param data_type a vector of strings, each entry needs to be either "agecomp" or "lengthcomp"
#'@param fleet_number a vector of numbers, each entry corresponds to which fleet the comp data are from
#'@param bins a list item, each item is a vector specifying either the age or length bins
#'@param caal_bool a vector of length \code{ss_data_list} with boolean values for whether the data is conditional age-at-length
SS_write_comps <- function(ss_data_list, comp_matrix,
                           data_rows, sampling_month,
                           data_type, fleet_number,
                           bins, caal_bool){

  #Check if age and length comp are in the data_type vector and remove all rows if so
  if("agecomp" %in% data_type){
    ss_data_list$agecomp <- ss_data_list$agecomp[0,]
    age_comp_ind <- which(data_type=="agecomp")
  }

  if("lencomp" %in% data_type){
    ss_data_list$lencomp <- ss_data_list$lencomp[0,-seq(7,nrow(ss_data_list$lencomp))]
    len_comp_ind <- which(data_type=="lencomp")
  }

  #Initialize local variables
  k <- 1
  start_year <- ss_data_list$styr

  for(i in 1:length(comp_matrix)){
    indices <- (k:(k+length(data_rows[[i]])-1))
    ss_data_list[[data_type[i]]][indices,"Yr"] <- data_rows[[i]]
    ss_data_list[[data_type[i]]][indices,"Seas"] <- sampling_month[[i]]
    ss_data_list[[data_type[i]]][indices,"FltSvy"] <- rep(fleet_number[i], length(data_rows[[i]]))

    ss_data_list[[data_type[i]]][indices,"Gender"] <-
      rep(0, length(data_rows[[i]]))

    ss_data_list[[data_type[i]]][indices,"Part"] <-
      rep(0,length(data_rows[[i]]))

    ss_data_list[[data_type[i]]][indices,"Nsamp"] <-
      comp_matrix[[i]][,"nsamp"]

    type_prefix <- switch(data_type[i],
                          "agecomp"="a",
                          "lencomp"="l")


    if(data_type[i]=="agecomp"){
      if(caal_bool[i]){

        ss_data_list[[data_type[i]]][indices,"Lbin_lo"] <- comp_matrix[[i]][,"lower.bins"]
        ss_data_list[[data_type[i]]][indices,"Lbin_hi"] <- comp_matrix[[i]][,"upper.bins"]
      } else{
      ss_data_list[[data_type[i]]][indices,"Lbin_lo"] <- rep(-1,length(data_rows[[i]]))
      ss_data_list[[data_type[i]]][indices,"Lbin_hi"] <- rep(-1,length(data_rows[[i]]))
      }
      ss_data_list[[data_type[i]]][indices, "Ageerr"] <- rep(1,length(data_rows[[i]]))
    }

  if(data_type[[i]]=="lencomp"){
      ss_data_list[[data_type[i]]][, paste(type_prefix,as.character(bins[[i]]), sep="")] <- 0
  }
  browser()
    ss_data_list[[data_type[i]]][indices,paste(type_prefix,as.character(bins[[i]]), sep="")] <- comp_matrix[[i]][,as.character(bins[[i]])]
}
    return(ss_data_list)

  }

