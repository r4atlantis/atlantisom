#' Calculate biomass in t per non-age-structured groups (biomass pools) per polygon and time.
#'
#' @template pooln
#' @template vol
#' @template area
#' @template fgs
#' @template biolprm
#' @return A \code{data.frame} in tidy format with the following columns:
#'   species, agecl=1, time, polygon and atoutput.
#'   Atoutput is the total biomass in t.
#'   At this point biomass was aggregated (sum) per layer or area for benthos!
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
calc_biomass_pool <- function(pooln, vol, area, fgs, biolprm){
  datalist <- list(pooln)

  # Conversion factor from mg N to t wet-weight
  # should only use conversion for non vertebrates
  # also need volume of cell info
  bio_conv <- biolprm$redfieldcn * biolprm$kgw2d / 1000000000

  # get fgs info
  # use grouptype column to allocate
  colnames(fgs) <- tolower(colnames(fgs))

  # check for GroupType or InvertType
  if (!("grouptype" %in% colnames(fgs) | "inverttype"%in% colnames(fgs))) {
    stop(paste("The columns GroupType or InvertType ars not in your functional\n",
               "groups file."))
  }

  # change inverttype to grouptype, contents should be the same
  if("inverttype" %in% names(fgs)) names(fgs)[names(fgs) == 'inverttype'] <- 'grouptype'

  fgs$grouptype <- tolower(fgs$grouptype)


  pooltype <- fgs |>
    dplyr::select(species=name, grouptype) |>
    dplyr::mutate(pooltype = dplyr::case_when((grouptype %in% c("lg_inf",
                                                                "sm_inf",
                                                                "sed_bact",
                                                                "sed_ep_ff",
                                                                "sed_ep_other",
                                                                "mobile_ep_other",
                                                                "coral",
                                                                "sponge")) ~ "benthos",
                                              TRUE ~ "nonbenth"))

  data_names <- c("species", "agecl", "polygon", "layer", "time", "atoutput")

  if (all(sapply(datalist, function(x) all(is.element(names(x), data_names))))){

    names(vol)[names(vol) == "atoutput"] <- "vol"

    sedlayer <- max(vol$layer)

    pooln <- merge(pooln, vol,
                   by = c("time", "polygon", "layer"))

    pooln <- merge(pooln, area,
                   by = c("polygon"))

    pooln <- merge(pooln, pooltype,
                   by = c("species"))

    #pooln$atoutput <- with(pooln, vol * atoutput * bio_conv)

    pooln <- pooln |>
      dplyr::mutate(atoutput = dplyr::case_when(pooltype == "nonbenth" ~ vol * atoutput * bio_conv,
                                                pooltype == "benthos" ~
                                                  ifelse(layer==sedlayer, area * atoutput * bio_conv, 0)))

    # Sum over layers
    biomass_pools <- aggregate(atoutput ~ species + agecl + time + polygon,
                               data = pooln, sum)
  } else {
    stop(paste("Dataframe names do not match with", data_names))
  }

  return(biomass_pools)
}
