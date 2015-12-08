#' Calculate biomass in t per age-structured-groups per polygon, time and ageclass.
#'
#'
#' @param nums Dataframe containing the numbers per functional-group ("species") per timesetp,
#' ageclass, layer and polygon. The dataframe has to origin from load_nc using "Nums" as select variable.
#' @param resn Dataframe containing the reserve nitrogen per functional-group ("species") per timesetp,
#' ageclass, layer and polygon. The dataframe has to origin from load_nc using "ResN" as select variable.
#' @param structn Dataframe containing the structural nitrogen per functional-group ("species") per timesetp,
#' ageclass, layer and polygon. The dataframe has to origin from load_nc using "StructN" as select variable.
#' @param prm_biol Connction of the biologial parameter file given as complete folder-filename string.
#' usually "[...]_biol.prm".
#' @return Dataframe in tidy format with the following columns: species, agecl, time, polygon and atoutput.
#' Atoutput is the total biomass in t. At this point biomass was aggregated (sum) per layer!

#' @details This functions converts the ATLANTIS output to a dataframe which can be processed in R.
#' @keywords gen
#' @export
#' @author Alexander Keth


calc_biomass_age <- function(nums, resn, structn, prm_biol){
  datalist <- list(nums, resn, structn)
  biolprm <- readLines(con = prm_biol)

  x_cn <- str_split_twice(biolprm[grep(pattern = "X_CN", x = biolprm)])
  k_wetdry <- str_split_twice(biolprm[grep(pattern = "k_wetdry", x = biolprm)])

  # Conversion factor from mg N to t wet-weight
  bio_conv <- x_cn * k_wetdry / 1000000000

  data_names <- c("species", "agecl", "polygon", "layer", "time", "atoutput")

  if (all(sapply(datalist, function(x) all(is.element(names(x), data_names))))){
    names(resn)[names(resn) == "atoutput"] <- "atresn"
    names(nums)[names(nums) == "atoutput"] <- "atnums"
    structn <- dplyr::inner_join(structn, nums)
    structn <- dplyr::inner_join(structn, resn)
    structn$biomass_ind <- with(structn, (atoutput + atresn) * atnums * bio_conv)

    biomass_ages <- structn %>%
      dplyr::group_by(species, agecl, time, polygon) %>%
      dplyr::summarise(atoutput = sum(biomass_ind))
  } else {
    stop(paste("Dataframe names do not match with", data_names))
  }

  return(biomass_ages)
}

