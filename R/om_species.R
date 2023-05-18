#'Species-specific atlantisom outputs
#'@description A wrapper function to reduce a set of \code{atlantisom} objects from atlantis
#'output to a single species or subset of species for creating assessment input datasets.
#'Optionally includes diet data (set diet=TRUE in function call, default is FALSE)
#'@param species The species to sample in the survey and fishery (a vector)
#'@param omlist output of \code{om_init}
#'@return Returns an omlist_ss list object containing dataframes and lists:
#' \itemize{
#'  \item{species_ss, species name(s)}
#'  \item{code_ss, species code from funct.groups}
#'  \item{truetotbio_ss, dataframe output of \code{load_bioind}, species_ss only}
#'  \item{truecatchbio_ss, dataframe output of \code{load_catch}, species_ss only}
#'  \item{YOY_ss, dataframe young of year output, code_ss only}
#'  \item{truenums_ss, numbers at age output of \code{run_truth}, species_ss only}
#'  \item{truebio_ss, biomass at age output of \code{run_truth}, species_ss only}
#'  \item{trueresn_ss, reserve nitrogen output of \code{run_truth}, species_ss only}
#'  \item{truestructn_ss, structural nitrogen output of \code{run_truth}, species_ss only}
#'  \item{truecatchnum_ss, fishery catch at age output of \code{run_truth}, species_ss only}
#'  \item{truecons_ss, total consumption output of \code{run_truth}, species_ss only}
#'  \item{truecatchtons_ss, fishery catch by fleet output of \code{run_truth}, code_ss only}
#'  \item{truedisctons_ss, fishery discard by fleet output of \code{run_truth}, code_ss only}
#'  \item{truenumsage_ss, true numbers at annual age output of \code{run_truth}, species_ss only}
#'  \item{truecatchage_ss, fishery catch at annual age output of \code{run_truth}, species_ss only}
#'  \item{truediscage_ss, fishery discard at annual age output of \code{run_truth}, species_ss only}
#'  \item{funct.groups_ss, dataframe of species characteristics, species_ss only}
#'  \item{biol, list of biological parameters passed from input omlist}
#'  \item{boxpars, dataframe of spatial parameters}
#'  \item{runpar, list of run parameters passed from input omlist}
#' },
#'
#'@export
#'
#'@family wrapper functions
#'@author Sarah Gaichas
#'
#'@examples
#'\dontrun{
#' # assuming CC3om is output of om_init(here("config/CC3config.r"))
#'CC3om_sardine <- om_species(c("Pacific_sardine"), CC3om)
#'
#'CC3om_2spp <- om_species(c("Pacific_sardine", "Mesopel_M_Fish"), CC3om)
#'}
#'
om_species <- function(species = spp, omlist, save = TRUE,
                       removefullom = TRUE){
  # spp format c("speciesname1", "speciesname2")
  if(!all(species %in% omlist$funct.group.names)) stop("species name not found")
  species_ss <- species

  #subset species true bio
  truetotbio_ss <- omlist$truetotbio[omlist$truetotbio$species %in% species_ss,]

  #subset species true catch
  truecatchbio_ss <- omlist$truecatchbio[omlist$truecatchbio$species %in% species_ss,]

  #subset species YOY
  # get code matching species name to split YOY file
  code_ss <- omlist$funct.groups$Code[which(omlist$funct.groups$Name %in% species_ss)]
  # cut to a single species in YOY file
  YOY_ss <- omlist$YOY %>%
    select(Time, paste0(code_ss, ".0"))
  # reformat to be like all the other objects

  # numbers at agecl at full resolution (all polygons and layers)
  truenums_ss <- omlist$truth$nums[omlist$truth$nums$species %in% species_ss,]

  # biomass at agecl at full resolution (all polygons and layers)
  truebio_ss <- omlist$truth$biomass_ages[omlist$truth$biomass_ages$species %in% species_ss,]

  # reserve nitrogen at agecl at full resolution
  trueresn_ss <- omlist$truth$resn[omlist$truth$resn$species %in% species_ss,]

  # structural nitrogen at agecl at full resolution
  truestructn_ss <- omlist$truth$structn[omlist$truth$structn$species %in% species_ss,]

  # catch in numbers at agecl at full resoluation (all polygons, no layer in output)
  truecatchnum_ss <- omlist$truth$catch[omlist$truth$catch$species %in% species_ss,]

  # catch in tons at full resolution by fleet (all polygons, no layer or agecl)
  truecatchtons_ss <- omlist$truth$catchtons[omlist$truth$catchtons$species %in% code_ss,]

  # discard in tons at full resolution by fleet (all polygons, no layer or agecl)
  truedisctons_ss <- omlist$truth$disctons[omlist$truth$disctons$species %in% code_ss,]

  # consumption (biomass_eaten) at agecl at full resolution (all polygons, no layer in output)
  # based on atlantisom::calc_pred_cons()
  truecons_ss <- omlist$truth$biomass_eaten[omlist$truth$biomass_eaten$species %in% species_ss,]

  # if annage output exists, add in, otherwise fill with NULL
  if("numsage" %in% names(omlist$truth)){
    truenumsage_ss <- omlist$truth$numsage[omlist$truth$numsage$species %in% species_ss,]
  } else {truenumsage_ss <- NULL}

  if("catchage" %in% names(omlist$truth)){
    truecatchage_ss <- omlist$truth$catchage[omlist$truth$catchage$species %in% species_ss,]
  } else {truecatchage_ss <- NULL}

  if("discage" %in% names(omlist$truth)){
    truediscage_ss <- omlist$truth$discage[omlist$truth$discage$species %in% species_ss,]
  } else {truediscage_ss <- NULL}

  # original biomass_eaten was not correct by polygon, and also appeared inflated
  # a separate set of functions will get true diet by polygon from the
  # DetailedDietCheck.txt file
  # now biomass_eaten is based on atlantisom::calc_pred_cons() and included above by default

  # if(diet){
  #   truebioeaten_ss <- omlist$truth$biomass_eaten[omlist$truth$biomass_eaten$species %in% species_ss,]
  # } else {truebioeaten_ss <- NULL}

  #subset species biol parameters? no, may miss some globals that aren't by species

  #subset species functional group info
  funct.groups_ss <- omlist$funct.groups[omlist$funct.groups$Code %in% code_ss,]

  #keep the runpar and also need boxes for survey selection

  omlist_ss <- list("species_ss" = species_ss,
                    "code_ss" = code_ss,
                    "truetotbio_ss" = truetotbio_ss,
                    "truecatchbio_ss" = truecatchbio_ss,
                    "YOY_ss" = YOY_ss,
                    "truenums_ss" = truenums_ss,
                    "truebio_ss" = truebio_ss,
                    "trueresn_ss" = trueresn_ss,
                    "truestructn_ss" = truestructn_ss,
                    "truecatchnum_ss" = truecatchnum_ss,
                    "truecons_ss" = truecons_ss,
                    "truecatchtons_ss" = truecatchtons_ss,
                    "truedisctons_ss" = truedisctons_ss,
                    "truenumsage_ss" = truenumsage_ss,
                    "truecatchage_ss" = truecatchage_ss,
                    "truediscage_ss" = truediscage_ss,
                    "funct.group_ss" = funct.groups_ss,
                    "biol" = omlist$biol,
                    "boxpars" = omlist$boxpars,
                    "runpar" = omlist$runpar)

  if(save){
    saveRDS(omlist_ss, file.path(d.name, paste0(scenario.name, "omlist_ss.rds")))
  }
  if(removefullom) rm(omlist) #not removing passed data object

  return(omlist_ss)
}
