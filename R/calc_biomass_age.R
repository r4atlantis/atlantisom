#' Calculate biomass in t per age-structured-groups per polygon, time and ageclass.
#'
#' @template nums
#' @template resn
#' @template structn
#' @template biolprm
#' @return A \code{data.frame} in tidy format with the following columns:
#'   species, agecl, time, polygon and atoutput.
#'   Atoutput is the total biomass in t.
#'   At this point biomass was aggregated (sum) per layer!

#' @details This functions converts the ATLANTIS output to a \code{data.frame}
#'   which can be processed in R.
#' @keywords gen
#' @export
#' @author Alexander Keth
#' @family calc functions

calc_biomass_age <- function(nums, resn, structn, biolprm){
  datalist <- list(nums, resn, structn)

  # Conversion factor from mg N to t wet-weight
  # should only use conversion for non vertebrates
  # also need volume of cell info
  bio_conv <- biolprm$redfieldcn * biolprm$kgw2d / 1000000000

  data_names <- c("species", "agecl", "polygon", "layer", "time", "atoutput")

  if (all(sapply(datalist, function(x) all(is.element(names(x), data_names))))){
    names(resn)[names(resn) == "atoutput"] <- "resn"
    names(structn)[names(structn) == "atoutput"] <- "structn"
    if (!any(is.element(names(nums), "layer"))) {
      # Calculate median individual weight over layers when catch data is used for numbers!
      structn <- aggregate(structn ~ species + agecl + polygon + time,
        data = structn, median)
      resn <- aggregate(resn ~ species + agecl + polygon + time,
        data = resn, median)
    }
    structn <- dplyr::left_join(nums, structn)
    structn <- dplyr::left_join(structn, resn)
    structn$atoutput <- with(structn, (structn + resn) * atoutput * bio_conv)

    biomass_ages <- aggregate(atoutput ~ species + agecl + time + polygon,
      data = structn, sum)
  } else {
    stop(paste("Dataframe names do not match with", data_names))
  }

  return(biomass_ages)
}

