#' @title Reformat compositional data for input in to Stock Synthesis
#'
#' @description to be added
#' @details to be added
#' @export
#'
#' @template comp_data
#' @param round.places
#' @param comp_type takes 3 possible values: lencomp, agecomp, caalcomp
#' @return flattened comp data set for input into SS
#'
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
  samp_size <- comp_proportion %>%
    group_by(time) %>% summarise("nsamp" = sum(atoutput))
  unmelt <- left_join(unmelt, samp_size, by =c("time"))
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
    samp_size <- comp_proportion %>%
      group_by(time, lower.bins, upper.bins) %>% summarise("nsamp" = sum(atoutput))
    unmelt <- left_join(unmelt, samp_size, by =c("time", "lower.bins","upper.bins"))

  }


  unmelt[is.na(unmelt)] <- 0

  #Sum across non-metadata columns to get sample sizes
  columns_nonsum <- c("fleet", "year","season",
                      "Lbin_lo","Lbin_hi", "month", "sex", "part", "ageerr","Nsamp", "time", "lower.bins","upper.bins")

  comp_flat<- unmelt[order(unmelt$time),]

  return(comp_flat)
}
