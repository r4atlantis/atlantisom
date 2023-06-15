#' Calculate biomass in t per non-age-structured groups (biomass pools) per polygon and time.
#'
#' @template pooln
#' @template vol
#' @template biolprm
#' @return A \code{data.frame} in tidy format with the following columns:
#'   species, agecl=1, time, polygon and atoutput.
#'   Atoutput is the total biomass in t.
#'   At this point biomass was aggregated (sum) per layer!
#'
#' @details This functions converts the ATLANTIS output to a \code{data.frame}
#'   which can be processed in R.
#' @keywords gen
#' @export
#' @author Alexander Keth modified by Sarah Gaichas
#' @family calc functions
#'
#' @examples
#' # Set up the example with input files
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' fgs <- load_fgs(d, "Functional_groups.csv")
#' bps <- load_bps(dir = d, fgs = "Functional_groups.csv",
#'   file_init = "Initial_condition.nc")
#' runprm <- load_runprm(d, "Run_settings.xml")
#' biolprm <- load_biolprm(dir = d,
#'   file_biolprm = "Biology.prm")
#' boxes <- get_boundary(load_box(dir = d, file_bgm = "Geography.bgm"))
#'
#' # Get the pool values
#' pooln <- load_nc(dir = d, file_nc = "outputs.nc",
#'   bps = bps, fgs = fgs, select_groups = fgs[fgs$IsTurnedOn > 0, "Name"],
#'   select_variable = "N", check_acronyms = TRUE, bboxes = boxes)
#'
#' biomasspools <- calc_biomass_pool(pooln = pooln, biolprm = biolprm)
#'
calc_biomass_pool <- function(pooln, vol, biolprm){
  datalist <- list(pooln)

  # Conversion factor from mg N to t wet-weight
  # should only use conversion for non vertebrates
  # also need volume of cell info
  bio_conv <- biolprm$redfieldcn * biolprm$kgw2d / 1000000000

  data_names <- c("species", "agecl", "polygon", "layer", "time", "atoutput")

  if (all(sapply(datalist, function(x) all(is.element(names(x), data_names))))){

    # units are mg N/m3 so need to expand to layer volume
    names(vol)[names(vol) == "atoutput"] <- "vol"

    pooln <- merge(pooln, vol,
                   by = c("time", "polygon", "layer"))

    pooln$atoutput <- with(pooln, vol * atoutput * bio_conv)

    # Sum over layers
    biomass_pools <- aggregate(atoutput ~ species + agecl + time + polygon,
                               data = pooln, sum)
  } else {
    stop(paste("Dataframe names do not match with", data_names))
  }

  return(biomass_pools)
}


