#'@description A wrapper function to reduce a set of \code{atlantisom} objects from atlantis
#'output to a single species or subset of species for creating assessment input datasets.
#'@param species The species to sample in the survey and fishery (a vector)
#'@param omlist output of \code{om_init}
#'@return Returns an omlist_ss list object containing dataframes and lists:
#' \itemize{
#'  \item{species_ss, species name(s)}
#'  \item{code_ss, species code from funct.groups}
#'  \item{truetotbio_ss, dataframe output of \code{load_bioind}, species_ss only}
#'  \item{truecatchbio_ss, dataframe output of \code{load_catch}, species_ss only}
#'  \item{YOY_ss, dataframe young of year output, species_ss only}
#'  \item{truenums_ss, numbers at age output of \code{run_truth}, species_ss only}
#'  \item{truebio_ss, biomass at age output of \code{run_truth}, species_ss only}
#'  \item{trueresn_ss, reserve nitrogen output of \code{run_truth}, species_ss only}
#'  \item{truestructn_ss, structural nitrogen output of \code{run_truth}, species_ss only}
#'  \item{truecatchnum_ss, fishery catch at age output of \code{run_truth}, species_ss only}
#'  \item{funct.groups_ss, dataframe of species characteristics, species_ss only}
#'  \item{biol, list of biological parameters passed from input omlist}
#'  \item{boxpars, dataframe o spatial parameters}
#'  \item{runpar, list of run parameters passed from input omlist}
#' },
#'
#'@export
#'
#'@family wrapper functions
#'@author Sarah Gaichas
#'
#'@examples
#' # assuming CC3om is output of om_init(here("config/CC3config.r"))
#'CC3om_sardine <- om_species(c("Pacific_sardine"), CC3om)
#'
#'CC3om_2spp <- om_species(c("Pacific_sardine", "Mesopel_M_Fish"), CC3om)
#'
#'
om_species <- function(species = spp, omlist, removefullom = TRUE){
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
                    "funct.group_ss" = funct.groups_ss,
                    "biol" = omlist$biol,
                    "boxpars" = omlist$boxpars,
                    "runpar" = omlist$runpar)

  if(removefullom) rm(omlist) #not removing passed data object

  return(omlist_ss)
}
