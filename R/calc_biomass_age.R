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

  x_cn <- as.numeric(as.character(biolprm$redfieldcn))
  k_wetdry <- as.numeric(as.character(biolprm$kgw2d))

  # Conversion factor from mg N to t wet-weight
  bio_conv <- x_cn * k_wetdry / 1000000000

  data_names <- c("species", "agecl", "polygon", "layer", "time", "atoutput")

  if (all(sapply(datalist, function(x) all(is.element(names(x), data_names))))){
    names(resn)[names(resn) == "atoutput"] <- "resn"
    names(structn)[names(structn) == "atoutput"] <- "structn"
    if (!any(is.element(names(nums), "layer"))) {
      # Calculate mean individual weight over layers when catch data is used for numbers!
      structn <- structn %>%
        dplyr::group_by(species, agecl, polygon, time) %>%
        dplyr::summarise(structn = mean(structn))
      resn <- resn %>%
        dplyr::group_by(species, agecl, polygon, time) %>%
        dplyr::summarise(resn = mean(resn))
    }
    structn <- dplyr::left_join(nums, structn)
    structn <- dplyr::left_join(structn, resn)
    structn$biomass_ind <- with(structn, (structn + resn) * atoutput * bio_conv)

    biomass_ages <- structn %>%
      dplyr::group_by(species, agecl, time, polygon) %>%
      dplyr::summarise(atoutput = sum(biomass_ind))
  } else {
    stop(paste("Dataframe names do not match with", data_names))
  }

  return(biomass_ages)
}

