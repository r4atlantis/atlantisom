#' Calculate eaten total biomass in tonnes for each functional group.
#'
#' Function to calculate the eaten total biomass in tonnes:
#' "consumption" for each functional group
#' per time, polygon, and ageclass.
#'
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
#'   species, agecl, time, polygon, atoutput (bio_eaten)
#'
#' @family calc functions
#' @importFrom magrittr %>%
#' @export
#' @author Alexander Keth, Sarah Gaichas
#'
#' @examples
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' fgs <- load_fgs(d, "Functional_groups.csv")
#' bps <- load_bps(dir = d, fgs = "Functional_groups.csv",
#'   file_init = "Initial_condition.nc")
#' runprm <- load_runprm(d, "Run_settings.xml")
#' boxes <- get_boundary(load_box(dir = d, file_bgm = "Geography.bgm"))
#' groups <- load_fgs(dir = d, "Functional_groups.csv")
#' groups <- groups[groups$IsTurnedOn > 0, "Name"]
#' eat <- load_nc(dir = d, file_nc = "outputsPROD.nc",
#'   bps = bps, fgs = fgs, select_groups = groups,
#'   select_variable = "Eat", check_acronyms = TRUE,
#'   bboxes = boxes)
#' grazing <- load_nc(dir = d, file_nc = "outputsPROD.nc",
#'   bps = bps, fgs = fgs, select_groups = groups,
#'   select_variable = "Grazing", check_acronyms = TRUE,
#'   bboxes = boxes)
#' vol <- load_nc_physics(dir = d, file_nc = "outputs.nc",
#'   physic_variables = "volume", aggregate_layers = FALSE,
#'   bboxes = boxes)
#' biolprm <- load_biolprm(dir = d,
#'   file_biolprm = "Biology.prm")
#' runprm <- load_runprm(dir = d, file_runprm = "Run_settings.xml")
#' calcs <- calc_pred_cons(eat = eat, grazing = grazing,
#'   vol = vol, biolprm = biolprm, runprm = runprm)
#' rm(calcs)
#'
calc_pred_cons <- function(eat, grazing, vol, fgs, biolprm, runprm){

  # Conversion factor from mg N to tonnes wet-weight
  bio_conv <- biolprm$redfieldcn * biolprm$kgw2d / 1000000000

  # Eat and grazing are per species, agecl, polygon, and time.
  # Therefore, we need to aggregate the vol over layers.
  # Eat calculated in atecology/atprocess.C
  # Eat( ) is defined here : \trunk\atlantis\atecology\atprocess.c(644)

  # SKG: The main question is the UNITS of eat and grazing.
  # manual says this is either mg/m3/day or mg/day/individ
  # calculations here assume mg/m3/day so multiplying by box vol
  # is this m3 assuming the entire box volume or only where groups feed?
  # do we need to match species to layers to get appropriate volume?
  # VERTnight_XXX and VERTday_XXX in biol.prm

  # biomass pool groups may get need volume expansion while
  # age structured are per capita by age group?

  # all layers? even sediment? try filtering out layer > 7 to remove sediment volume
  # WARNING: ensure that layer 7 (processed via atlantisom) isn't unique to NOBA
  # layer 7 is unique to NOBA, use max of layers as sediment layer
  # however, unclear this is necessary, try without

  colnames(fgs) <- tolower(colnames(fgs))

  # check for GroupType or InvertType
  if (!("grouptype" %in% colnames(fgs) | "inverttype"%in% colnames(fgs))) {
    stop(paste("The columns GroupType or InvertType ars not in your functional\n",
               "groups file."))
  }

  # change inverttype to grouptype, contents should be the same
  if("inverttype" %in% names(fgs)) names(fgs)[names(fgs) == 'inverttype'] <- 'grouptype'

  fgs$grouptype <- tolower(fgs$grouptype)

  # is there a different calculation for benthos? don't expand to volume?
  # not using this yet
  pooltype <- fgs |>
    dplyr::select(species=name, grouptype) |>
    dplyr::mutate(pooltype = dplyr::case_when((grouptype %in% c("lg_inf",
                                                                "sm_inf",
                                                                "sed_bact",
                                                                "sed_ep_ff",
                                                                "sed_ep_other",
                                                                "mob_ep_other",
                                                                "coral",
                                                                "sponge")) ~ "benthos",
                                              (grouptype %in% c("lg_phy")) ~ "lg_phy",
                                              TRUE ~ "alllayers"))
  # not using this either
  sedlayer <- max(vol$layer)

  vol <- aggregate(atoutput ~ polygon + time, data = vol, sum)
  colnames(vol)[colnames(vol) == "atoutput"] <- "vol"

  # Combine eat and grazing! Calculate eaten biomass
  biomass_eaten <- rbind(eat, grazing)
  biomass_eaten <- merge(biomass_eaten, vol,
    by = c("time", "polygon"))

  # todo: previously AK noted that the bio_eaten values were too high
  # the new values need to be checked to see if they give realistic
  # results
  # removed layer 7 in volume which should expand mg N to only water?
  # total consumption and per capita from this look ok
  # snapshot so should be daily value?

  biomass_eaten$bio_eaten <- with(biomass_eaten,
    atoutput * vol * bio_conv)

  biomass_eaten$atoutput <- biomass_eaten$bio_eaten

  return(biomass_eaten)
}
