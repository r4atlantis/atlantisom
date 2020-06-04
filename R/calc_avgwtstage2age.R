#' Approximating the average weight at true age from stage-specific average weight.
#'
#' @family calc functions
#' @author Sarah Gaichas
#'
#' @param wtagecl average weight at age output of the \code{calc_age2length} function
#' @param annages truenumsage output of \code{run_truth}
#' @template fgs
#'
#' @return A \code{data.frame} in long format with the following column names:
#'   species, time, polygon, agecl, and atoutput (i.e., variable). This is
#'   the same as the input Nums data frame however, agecl will be TRUE numbers
#'   at annual age -- just named the same as the input to ensure make it work
#'   with the other functions
#'
#' @export
#'
#' @examples
#' dir <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file_nc="outputSETAS.nc"
#' fgs=load_fgs(dir = dir, "functionalGroups.csv")
#' file_init="INIT_VMPA_Jan2015.nc"
#' bps=load_bps(dir = dir, fgs, file_init)
#' select_groups=fgs$Name[fgs$IsTurnedOn > 0]
#' select_variable="Nums"
#' box.info=load_box(dir = dir, file_bgm="VMPA_setas.bgm")
#' bboxes=get_boundary(box.info)
#' #when calc_stage2age is run in the run_truth, it will need to have the nums
#' #data frame and the bioprm already read in:
#' nums_data <- load_nc(dir = dir,
#'                      file_nc="outputSETAS.nc",
#'                      bps=bps, fgs=fgs, select_groups=select_groups,
#'                      select_variable = "Nums",
#'                      check_acronyms = TRUE, bboxes = bboxes)
#' biolprm <- load_biolprm(dir, file_biolprm="VMPA_setas_biol_fishing_Trunk.prm")
#' YOY <- load_yoy(dir, file_yoy="outputSETASYOY.txt")

## ACTUAL FUNCTION ##
calc_avgwtstage2age <- function(wtagecl, annages, fgs) {
  # wtagecl is mean weight at age output of calc_age2length()
  # annages is truenumsage output of run_truth(), needs aggregation:
  annages <- annages %>%
    group_by(species, time, agecl) %>%
    rename(trueage = agecl) %>%
    summarise(truenatage = sum(atoutput))

  # assuming fgs is data object already been read in by load_fgs elsewhere
  # Figure out the groups that have multiple ages classes in each stage (or
  # cohort)
  multiple_ages <- fgs[fgs$NumAgeClassSize>1, c(1,4,10)]

  # match species in wtagecl_data to those species in the fgs file
  sppwt <- levels(wtagecl$species)
  multiple_ages <- multiple_ages[multiple_ages$Name %in% sppwt, ]

  # make a dataframe the right dimensions for all age classes
  # has species, time from wtage and trueage, truenatage from annages
  # add agecl column
  # finds the average age of each agecl each timestep based on truenatage
  wtage_out <- annages %>%
    dplyr::semi_join(wtagecl, by = c("species", "time")) %>%
    dplyr::left_join(multiple_ages, by = c("species" = "Name")) %>%
    dplyr::mutate(agecl = as.integer(ceiling(trueage/NumAgeClassSize))) %>%
    dplyr::group_by(species, time, agecl) %>%
    dplyr::mutate(avgage = weighted.mean(trueage, truenatage))

  wtage_avgage <- wtage_out %>%
    select(species, time, agecl, avgage) %>%
    distinct()

  # find weight increment for a timestep
  # muweight for agecl+1 - muweight for agecl
  wtagecl_inc <- wtagecl %>%
    dplyr::arrange(species, time, agecl) %>%
    #dplyr::left_join(multiple_ages, by = c("species" = "Name")) %>%
    dplyr::group_by(species, time) %>%
    dplyr::mutate(increment = case_when(
      agecl==1 ~ atoutput,
      agecl>1 ~ atoutput - dplyr::lag(atoutput)
    )) %>%
    dplyr::left_join(wtage_avgage) %>%
    dplyr::mutate(avgageinc = case_when(
      agecl==1 ~ avgage,
      agecl>1 ~ avgage - dplyr::lag(avgage)
    ))

  # complex, but works for 2 ages/agecl, need to test more, need to fix oldest
  # for more than 2 ages/agecl need to have a bigger lead/lag
  # achieved by faking a scalar for lead/lag n using max() function
  # should be ceiling I think for odd NumAgeClassSize, but not sure?
  wtage_out <- wtage_out %>%
    dplyr::select(species, time, trueage, NumAgeClassSize, agecl, avgage) %>%
    dplyr::left_join(wtagecl_inc) %>%
    dplyr::group_by(species, time) %>%
    dplyr::mutate(wtIntage = case_when(
      (agecl==1 & trueage<avgage) ~ (1-(avgage-trueage)/avgageinc)*increment,
      (agecl>1 & trueage<avgage) ~
        (1-(avgage-trueage)/avgageinc)*increment +
        lag(atoutput, ceiling(max(NumAgeClassSize)/2)),
      (trueage>avgage) ~
        ((trueage-avgage)/lead(avgageinc, ceiling(max(NumAgeClassSize)/2)))*lead(increment, ceiling(max(NumAgeClassSize)/2)) + atoutput
    ))

  # fix oldest age

  # diagnostic plot--are interpolations where we expect them?
  wtageclann <- ggplot(wtagecl_inc, aes(avgage, atoutput)) +
    geom_point() +
    geom_line(aes(colour = factor(time))) +
    scale_x_continuous(minor_breaks = c(0:max(wtage_out$trueage))) +
    theme_tufte() +
    theme(legend.position = "none",
          panel.grid.minor.x = element_line(colour = "grey50"),
          panel.grid.major.x = element_line(colour = "grey50"))

  diag.p <- wtageclann + geom_point(data = wtage_out,
                          mapping = aes(trueage, wtIntage))

  #clean up to have only standard columns
  finaldata <- data.frame("species" = wtage_out$species,
                          "agecl" = wtage_out$trueage, "polygon" = NA, "layer" = NA,
                          "time" = wtage_out$time, "atoutput" = wtage_out$wtIntage)

  return(list(finaldata, diag.p))

}

