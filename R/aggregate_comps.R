#'Function to aggregate compositions into a shorter range of bins based on  data availability in each bin
#'@export
#'@param comp.data output from the \code{calc_age2length} function, namely a list with objects mulen, muweight, and natlength
#'@param compbins the age or length composition bins you want to compress the data into, either this or compression can be specified but not both
#'@param compression the minimum thresshold for numbers of fish in each bin for the bin to be included in the data, default is 1
aggregate_comps <- function(comp.data,
                            compbins = NULL,
                            compression = 1,
                            proportion.thresh = 0.9){

  #Use compression if bins are not user-specified
  if(is.null(compbins)){
    rowind<- which(comp.data$natlength$atoutput<compression)
    summarize.bins <- as.data.frame(table(comp.data$natlength[rowind,]$upper.bins))
    summarize.total.bins <- as.data.frame(table(comp.data$natlength$upper.bins))

    combined.bins <-left_join(x = summarize.total.bins,
              y = summarize.bins, by = "Var1") %>%
      mutate(proportion = Freq.y/Freq.x)

    potential_removal <- which(combined.bins$proportion>proportion.thresh)
    non_sequential <- which(diff(potential_removal)!=1)
    actual_removal <- potential_removal[-non_sequential[seq(2,length(non_sequential),by=2)]]

  } else{
    stop("Error: ability to specify user bins is not yet implemented.")
  }

  times <- unique(comp.data$natlength$time)
  ageclasses <- unique(comp.data$natlength$agecl)
  #loop through and aggregate fish smaller than smallest bin in smallest bin
  for(i in 1:length(times)){
    working_data <- filter(comp.data$natlength,
                           time == times[i])

#Number of fish in the smallest and the biggest bin
    n_smallest <- n_biggest <- rep(0,
                                length(ageclasses))

    min_upper_bin <- which(diff(actual_removal)!=1)
    max_upper_bin <- actual_removal[which(actual_removal==min_upper_bin)+1]
    min_upper_bin <- min_upper_bin + 1

    aggregate_bins <- function(x, df, upper_lower){
      age_d <- filter(working_data,
                      agecl==x)
      n <- filter(age_d,
                  upper.bins < min_upper_bin) %>%
        select(atoutput) %>% sum()
      return(n)

    }

    foo <- lapply(ageclasses, aggregate_bins, df=working_data, upper_lower = "upper")
    bar <- lapply(ageclasses, aggregate_bins, df=working_data, upper_lower = "lower")


  }
}
