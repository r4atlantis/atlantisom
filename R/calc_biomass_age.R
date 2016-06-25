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
#'
#' @details This functions converts the ATLANTIS output to a \code{data.frame}
#'   which can be processed in R.
#' @keywords gen
#' @export
#' @author Alexander Keth
#' @family calc functions
#'
#' @examples
#' # Set up the example with input files
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' fgs <- load_fgs(d, "functionalGroups.csv")
#' bps <- load_bps(dir = d, fgs = fgs,
#'   file_init = paste0(tail(strsplit(d, "/")[[1]], 1), ".nc"))
#' runprm <- load_runprm(d, "VMPA_setas_run_fishing_F_Trunk.xml")
#' biolprm <- load_biolprm(dir = d,
#'   file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")
#' boxes <- get_boundary(load_box(dir = d, file_bgm = "VMPA_setas.bgm"))
#'
#' # Get the catch values
#' catch <- load_nc(dir = d, file_nc = "outputSETASCATCH.nc",
#'   bps = bps, fgs = fgs, select_groups = fgs[fgs$IsTurnedOn > 0, "Name"],
#'   select_variable = "Catch", check_acronyms = TRUE, bboxes = boxes)
#' structn <- load_nc(dir = d, file_nc = "outputSETAS.nc",
#'   bps = bps, fgs = fgs, select_groups = fgs[fgs$IsTurnedOn > 0, "Name"],
#'   select_variable = "StructN", check_acronyms = TRUE, bboxes = boxes)
#' resn <- load_nc(dir = d, file_nc = "outputSETAS.nc",
#'   bps = bps, fgs = fgs, select_groups = fgs[fgs$IsTurnedOn > 0, "Name"],
#'   select_variable = "ResN", check_acronyms = TRUE, bboxes = boxes)
#'
#' biomassatage <- calc_biomass_age(nums = catch,
#'   resn = resn, structn = structn, biolprm = biolprm)
#'
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
    structn <- merge(nums, structn)
    structn <- merge(structn, resn)
    structn$atoutput <- with(structn, (structn + resn) * atoutput * bio_conv)

    biomass_ages <- aggregate(atoutput ~ species + agecl + time + polygon,
      data = structn, sum)
  } else {
    stop(paste("Dataframe names do not match with", data_names))
  }

  return(biomass_ages)
}

