#' Calculate eaten biomass in t for each functional group.
#'
#' Function to calculate the eaten biomass in t for each functional group per
#' time, polygon, ageclass and preyitem. Please note, that the function
#' produces unrealistic high consumption values at the momemt.
#' @param dietcomp Dataframe containing the diet proportion for each functional group
#' per prey, time and ageclass. The dataframe has to origin from load_diet_comp using the
#' diet_check.txt as input.
#' @param eat Dataframe containing the consumed biomass in mg N for each functional-group ("species")
#' per timesetp, ageclass, and polygon. The dataframe has to origin from load_nc using "Eat" as select variable.
#' @param grazing Dataframe containing the consumed biomass in mg N for each functional-group ("species")
#' per timesetp, ageclass, and polygon. The dataframe has to origin from load_nc using "Grazing" as select variable.
#' @param prm_biol Connction of the biologial parameter file given as complete folder-filename string.
#' usually "[...]_biol.prm".
#' @return Dataframe in tidy format with the following columns: species, agecl, polygon, time,
#' atoutput, vol, dietcomp, prey, bio_eaten.

#' @export
#' @author Alexander Keth

calc_pred_diet <- function(dietcomp, eat, grazing, vol, prm_biol){
  dietcomp <- dplyr::filter(dietcomp, dietcomp > 0)

  # Eat and razing is given per species, agecl, polygon and time.
  # Therefore, we need to aggreate the vol over layers.
  vol <- vol %>%
    dplyr::group_by(polygon, time) %>%
    dplyr::summarise(vol = sum(atoutput))

  # Combine eat and grazing! Calculte eaten biomass
  biomass_eaten <- rbind(eat, grazing) %>%
    dplyr::left_join(vol) %>%
    dplyr::left_join(dietcomp, by = c("species", "agecl", "time"))

  biolprm <- readLines(con = prm_biol)

  x_cn <- str_split_twice(biolprm[grep(pattern = "X_CN", x = biolprm)])
  k_wetdry <- str_split_twice(biolprm[grep(pattern = "k_wetdry", x = biolprm)])

  # Conversion factor from mg N to t wet-weight
  bio_conv <- x_cn * k_wetdry / 1000000000

  # NOTE: Although there are observations for eat/grazing information
  # of dietcomp is missing...
  result <- biomass_eaten %>%
    dplyr::filter(!is.na(dietcomp)) %>%
    dplyr::mutate(bio_eaten = atoutput * vol * dietcomp * bio_conv)

  # NOTE: biomass eaten is given in tonnes... The values are too high!

  return(result)
}



