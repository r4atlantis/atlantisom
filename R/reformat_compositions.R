reformat_compositions <- function(comp_data,
                                  round.places = 4,
                                  comp_type){
  if(comp_type=="lencomp"){
    comp_proportion <- comp_data %>%
      group_by(time, lower.bins, upper.bins) %>%
      summarise(comp = sum(atoutput))
  } else{
  comp_proportion <- comp_data %>%
    group_by(time) %>%
    mutate(comp = round(atoutput/sum(atoutput),round.places))
  }

  if(comp_type=="agecomp"){
  unmelt <- dcast(data = comp_proportion,
                  formula = time ~agecl,
                  value.var = "comp")
  }

  if(comp_type=="lencomp"){
    unmelt <- dcast(data = comp_proportion,
                    formula = time ~lower.bins,
                    value.var = "comp")
  }

  if(comp_type=="caalcomp"){

    unmelt <- dcast(data = comp_proportion,
                    formula = time + lower.bins + upper.bins ~agecl,
                    value.var = "comp")
  }


  unmelt[is.na(unmelt)] <- 0

  #Sum across non-metadata columns to get sample sizes
  columns_nonsum <- c("fleet", "year","season",
                      "Lbin_lo","Lbin_hi", "month", "sex", "part", "ageerr","Nsamp", "time", "lower.bins","upper.bins")

  sample_size <- apply(unmelt[,(!names(unmelt)%in% columns_nonsum)], FUN=sum, MARGIN=1)
  comp_flat<- cbind(unmelt[order(unmelt$time),],
                        sample_size)
  names(comp_flat)[ncol(comp_flat)] <- "nsamp"

  return(comp_flat)
}
