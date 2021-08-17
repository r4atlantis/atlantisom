#' @title Sample total consumption to create diet composition data
#'
#' @description Create sampled diet composition data from the total consumption
#'    in an Atlantis scenario. Observation error and bias are added.
#'
#' @details The function takes diet composition data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{load_diet_comp} or
#'   \code{load_detailed_diet_comp}.
#'   One does not need to use these functions to create \code{dat}, rather you must only
#'   ensure that the structure of \code{dat} is the same.
#'   The user supplies a parameter \code{unidprey} ranging from 0 to 1 that determines
#'   bias in sample diet composition. Bias is added by allocating a random portion of
#'   each group to an "unidentified prey" category. The default \code{unidprey} is 0
#'   which results in no reallocation of prey to unidentified categories.
#'   The user supplies a parameter \code{alphamult} that detemines observation error
#'   for sample diet compostion strays using a dirichlet distribution. The default
#'   \code{alphamult} is 10000000 which results in minimal observation error in diet
#'   composition. Lower values of \code{alphamult} increase observation error.
#'   The function adjusts the remaining diet so each predator diet per agecl (if in
#'   the input data) and time.days sums to one.

#' @author Robert Wildermuth, Sarah Gaichas
#' @importFrom magrittr %>%
#' @export
#' @param dat A \code{data.frame} containing species (predator), agecl, time.days,
#'  atoutput (diet proportion) and prey species
#' @template fgs
#' @param alphamult default (10000000) returns true diet comps, while a
#'   lower number (~10) returns high variance diet comp
#' @param unidprey default (0) returns correctly identified prey, while
#'   decimal values up to 1 allocate up to that proportion of prey to an unidentified
#'   category at random.
#'
#' @examples
#' \dontrun{
#'		d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#'		groups <- load_fgs(dir = directory, "Functional_groups.csv")
#'		groups <- groups[groups$IsTurnedOn > 0, "Name"]
#'		results <- run_truth(scenario = "outputs",
#'		dir = d,
#'		file_fgs = "Functional_groups.csv",
#'		file_bgm = "Geography.bgm",
#'		select_groups = groups,
#'		file_init = "Initial_condition.nc",
#'		file_biolprm = "Biology.prm",
#'		file_runprm = "Run_settings.xml",
#'    file_fish = "Fisheries.csv")
#'
#'		# rows should each sum to one:
#'		rowSums(dat[,2:NCOL(dat)])
#'		dim(dat)
#'
#'		obsDietComp <- sample_diet(dat)
#'		dim(obsDietComp)
#'}

sample_diet <- function(dat, fgs, unidprey = 0, alphamult = 10000000) {

  # dat can be long format output of load_diet_comp, or create_survey_diet, both have layers aggregated
  # load_diet_comp output has no polygon column and atoutput is proportion,
  # create_survey_diet output has polygon column and atoutput is consumption in tons
  # first aggregate to get global diet comp by species, agecl, time in proportion, if not already in that form

  if("polygon" %in% names(dat)){
    #sum over boxes and assume sampling occurs coastwide (the sampled boxes were already subset in create functions)
    dat2 <- aggregate(dat$atoutput,list(dat$species,dat$agecl,dat$time, dat$prey),sum)
    names(dat2) <- c("species","agecl","time","prey", "sumcons")

    dat2 <- dat2 %>%
      dplyr::group_by(species, agecl, time) %>%
      dplyr::mutate(totcons = sum(sumcons)) %>%
      dplyr::mutate(dietprop = sumcons/totcons) %>%
      dplyr::arrange(species, agecl, time)

    # rename to match output of load_diet_comp
    names(dat2)[names(dat2) == 'dietprop'] <- 'atoutput'
    names(dat2)[names(dat2) == 'time'] <- 'time.days'

  } else dat2 <- dat

  # leave as age class specific

  # then apply id/aggregation bias--this adds prey categories that don't exist??
  # user specified max proportion to reallocate
  # apply taxonomically, allocating fish prey to fish unid, invert prey to invert unid, then some portion unid
  # phytoplankton and zooplankton prey direct to unid?

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

  # associate prey categories (atlantis user guide I Table 8) with unid categories
  fgs<- fgs %>%
    dplyr::mutate(unidtype = dplyr::case_when((grouptype %in% c("mammal",
                                                  "bird",
                                                  "reptile")) ~ "Unid_Vert",
                                (grouptype %in% c("fish",
                                                  "shark")) ~ "Unid_Fish",
                                (grouptype %in% c("fish_invert",
                                                  "cep",
                                                  "pwn",
                                                  "lg_inf",
                                                  "sm_inf",
                                                  "sed_ep_ff",
                                                  "sed_ep_other",
                                                  "mobile_ep_other",
                                                  "coral",
                                                  "sponge")) ~ "Unid_Invert",
                                (grouptype %in% c("lg_zoo",
                                                  "med_zoo",
                                                  "sm_zoo",
                                                  "dinoflag",
                                                  "lg_phy",
                                                  "sm_phy")) ~ "Unid_Plankton",
                                TRUE ~ "Unid"))

  # associate prey with prey category
  # randomly assign up to unidprey % of prey in each prey category to associated unid category
  # take remainder for identified prey

  fgsprey <- fgs[,c("name", "unidtype")]
  names(fgsprey)[names(fgsprey) == 'name'] <- 'prey'

  dat2 <- dplyr::left_join(dat2, fgsprey) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(unidprop = runif(1, 0, unidprey*atoutput),
           sampprop = atoutput - unidprop)

  # add rows with unid categories and sum prop assigned to the category

  unidrows <- dat2 %>%
    #select(-atoutput, -prey, -sampprop) %>%
    dplyr::group_by(species, time.days, agecl, unidtype) %>%
    dplyr::summarise(unidtot = sum(unidprop)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(prey = unidtype, sampprop = unidtot)

  dat2 <- dat2 %>%
    dplyr::select(species, time.days, agecl, prey, sampprop) %>%
    dplyr::full_join(unidrows)

  # then apply Dirichlet obs error; Dirichlet equivalent of rmultinom(1, effN, trueagecomp)?
  # see e.g., https://rdrr.io/cran/DirichletReg/man/Dirichlet.html
  # alpha vector is the diet comp scaled by a user-supplied multiplier
  # default to a very high multiplier for close-to-true diet comp
  # lower multipliers stray further from true diet comp, alpha<1 goes way off

  #tidy
  if(utils::packageVersion("dplyr") < "0.9.9"){
    dat2 <- dat2 %>%
      dplyr::filter(sampprop>0) %>%
      dplyr::group_by(species, time.days, agecl) %>%
      dplyr::do(mutate(., dietSamp = DirichletReg::rdirichlet(1, alphamult * sampprop))) %>%
      dplyr::ungroup() %>%
      dplyr::select(-sampprop)
  }

  #works for dplyr 1.0 and up; if-else statements within pipe not working
  if(utils::packageVersion("dplyr") > "0.9.9"){
    dat2 <- dat2 %>%
      dplyr::filter(sampprop>0) %>%
      dplyr::group_by(species, time.days, agecl) %>%
      dplyr::group_map(~mutate(., dietSamp = as.vector(t(DirichletReg::rdirichlet(1, alphamult * .$sampprop)))),
                       .keep = T) %>%
      dplyr::bind_rows() %>%
      dplyr::ungroup() %>%
      dplyr::select(-sampprop)
  }

  # returns diet proportions with observation error: Dirichlet, and bias from unknown id

  return(dat2)
}
