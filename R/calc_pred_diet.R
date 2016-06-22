#' Calculate eaten biomass in tonnes for each functional group.
#'
#' Function to calculate the eaten biomass in tonnes for each functional group
#' per time, polygon, ageclass, and prey item.
#'
#' @param dietcomp A \code{data.frame} containing the diet proportions for
#'   each functional group, prey functional group, time, and ageclass.
#'   The \code{data.frame} must originate from \code{\link{load_diet_comp}}
#'   using the \code{diet_check.txt} as input.
#' @param eat A \code{data.frame} containing the consumed biomass in mg N for
#'   each functional group, time, ageclass, and polygon.
#'   The \code{data.frame} must originate from \code{\link{load_nc}}
#'   using \code{select_variable = "Eat"}.
#' @param grazing A \code{data.frame} containing the consumed biomass in mg N for
#'   each functional group, time, ageclass, and polygon.
#'   The \code{data.frame} must originate from \code{\link{load_nc}}
#'   using \code{select_variable = "Grazing"}.
#' @param vol A \code{data.frame} containing the volume for
#'   each time, polygon, and layer.
#'   The \code{data.frame} must originate from \code{\link{load_nc_physics}}
#'   using \code{physic_variables = "volume"}.
#' @template biolprm
#' @return A code{data.frame} in tidy format with the following columns:
#'   species, agecl, time, polygon, atoutput, vol, prey, dietcomp, bio_eaten
#'
#' @family calc functions
#' @export
#' @author Alexander Keth
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' fgs <- load_fgs(d, "functionalGroups.csv")
#' bps <- load_bps(dir = d, fgs = fgs,
#'   file_init = paste0(tail(strsplit(d, "/")[[1]], 1), ".nc"))
#' runprm <- load_runprm(d, "VMPA_setas_run_fishing_F_Trunk.xml")
#' dietcomp <- load_diet_comp(dir = d, file_diet = "outputSETASDietCheck.txt",
#'   fgs = fgs, toutinc = runprm$toutinc)
#' boxes <- get_boundary(load_box(dir = d, file_bgm = "VMPA_setas.bgm"))
#' groups <- load_fgs(dir = d, "functionalGroups.csv")
#' groups <- groups[groups$IsTurnedOn > 0, "Name"]
#' eat <- load_nc(dir = d, file_nc = "outputSETASPROD.nc",
#'   bps = bps, fgs = fgs, select_groups = groups,
#'   select_variable = "Eat", check_acronyms = TRUE,
#'   bboxes = boxes)
#' grazing <- load_nc(dir = d, file_nc = "outputSETASPROD.nc",
#'   bps = bps, fgs = fgs, select_groups = groups,
#'   select_variable = "Grazing", check_acronyms = TRUE,
#'   bboxes = boxes)
#' vol <- load_nc_physics(dir = d, file_nc = "outputSETAS.nc",
#'   physic_variables = "volume", aggregate_layers = FALSE,
#'   bboxes = boxes)
#' biolprm <- load_biolprm(dir = d,
#'   file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")
#' calcs <- calc_pred_diet(dietcomp = dietcomp, eat = eat, grazing = grazing,
#'   vol = vol, biolprm = biolprm)
#' rm(calcs)
#'
calc_pred_diet <- function(dietcomp, eat, grazing, vol, biolprm){

  # Conversion factor from mg N to tonnes wet-weight
  bio_conv <- biolprm$redfieldcn * biolprm$kgw2d / 1000000000

  dietcomp <- dietcomp[dietcomp$dietcomp > 0, ]

  # Eat and grazing are per species, agecl, polygon, and time.
  # Whereas, the dietcomp data do not have polygon.
  # Therefore, we need to aggregate the vol over layers.
  vol <- aggregate(atoutput ~ polygon + time, data = vol, sum)
  colnames(vol)[colnames(vol) == "atoutput"] <- "vol"

  # Combine eat and grazing! Calculate eaten biomass
  biomass_eaten <- rbind(eat, grazing)
  biomass_eaten <- merge(biomass_eaten, vol,
    by = c("time", "polygon"))

  # todo: change diet to age class rather than cohort
  # todo: previously AK noted that the bio_eaten values were too high
  # the new values need to be checked to see if they give realistic
  # results

  # merge the biomass eaten and diet composition information
  # although there will be observations for eating and grazing,
  # not all rows will have associated diet composition information
  # these rows are removed, but could be added as na if the argument
  # by.x = TRUE were added to the following function call.
  biomass_eaten <- merge(biomass_eaten, dietcomp,
    by.x = c("species", "agecl", "time"),
    by.y = c("species", "cohort", "time"))
  biomass_eaten$bio_eaten <- with(biomass_eaten,
    atoutput * vol * dietcomp * bio_conv)

  return(biomass_eaten)
}
