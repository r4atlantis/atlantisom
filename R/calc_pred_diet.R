#' Calculate eaten biomass in tonnes for each functional group.
#'
#' Function to calculate the eaten biomass in tonnes for each functional group
#' per time, polygon, ageclass, and prey item.
#' todo: fix calc_pred_diet, which currently
#' produces unrealistic high consumption values.
#'
#' @param dietcomp Dataframe containing the diet proportion for each functional group
#' per prey, time and ageclass. The dataframe has to origin from load_diet_comp using the
#' diet_check.txt as input.
#' @param eat Dataframe containing the consumed biomass in mg N for each functional-group ("species")
#' per timesetp, ageclass, and polygon. The dataframe has to origin from load_nc using "Eat" as select variable.
#' @param grazing Dataframe containing the consumed biomass in mg N for each functional-group ("species")
#' per timesetp, ageclass, and polygon. The dataframe has to origin from load_nc using "Grazing" as select variable.
#' @param vol todo: describe volume, not sure where this comes from.
#' @template biolprm
#' @return Dataframe in tidy format with the following columns: species, agecl, polygon, time,
#' atoutput, vol, dietcomp, prey, bio_eaten.
#'
#' @family calc functions
#' @export
#' @author Alexander Keth
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' fgs <- load_fgs(d, "functionalGroups.csv")
#' dietcomp <- load_diet_comp(dir = d, file_diet = "outputSETASDietCheck.txt",
#'   fgs = fgs)
#' eat <- NULL
#' grazing <- NULL
#' vol <- NULL
#' biolprm <- load_biolprm(dir = d,
#'   file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")
#' calcs <- calc_pred_diet(dietcomp = dietcomp, eat = eat, grazing = grazing,
#'   vol = vol, biolprm = biolprm)
#' rm(calcs)
#'
calc_pred_diet <- function(dietcomp, eat, grazing, vol, biolprm){
  dietcomp <- dplyr::filter(dietcomp, dietcomp > 0)

  # Eat and grazing are per species, agecl, polygon, and time.
  # Therefore, we need to aggregate the vol over layers.
  vol <- vol %>%
    dplyr::group_by(polygon, time) %>%
    dplyr::summarise(vol = sum(atoutput))
  # todo: change to aggregate
  # vol <- aggregate(atoutput ~ polygon + time, data = vol, sum)

  # Combine eat and grazing! Calculte eaten biomass
  biomass_eaten <- rbind(eat, grazing) %>%
    dplyr::left_join(vol) %>%
    dplyr::left_join(dietcomp, by = c("species", "agecl", "time"))

  # Conversion factor from mg N to tonnes wet-weight
  bio_conv <- biolprm$redfieldcn * biolprm$kgw2d / 1000000000

  # NOTE: Although there are observations for eat/grazing information
  # of dietcomp is missing...
  result <- biomass_eaten %>%
    dplyr::filter(!is.na(dietcomp)) %>%
    dplyr::mutate(bio_eaten = atoutput * vol * dietcomp * bio_conv)

  # NOTE: biomass eaten is given in tonnes... The values are too high!

  return(result)
}



