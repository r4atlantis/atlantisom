#'Initializes atlantisom
#'@description A wrapper function to initialize an \code{atlantisom} objects from atlantis
#'output prior to creating assessment input datasets. This makes a large object!
#'@param config file specifying directory with atlantis model outputs and identifying key files
#'@return Returns an omlist list object containing dataframes and lists:
#' \itemize{
#'  \item{funct.groups, dataframe of species characteristics}
#'  \item{funct.group.names, vector of species names}
#'  \item{truetotbio, dataframe output of \code{load_bioind}}
#'  \item{truecatchbio, dataframe output of \code{load_catch}}
#'  \item{YOY, dataframe young of year output}
#'  \item{biol, list of biological parameters output of \code{load_biolprm}}
#'  \item{runpar, list of run parameters output of \code{load_runprm}}
#'  \item{boxpars, dataframe o spatial parameters}
#'  \item{truth, list of dataframes, output of \code{run_truth}}
#' },
#'the atlantisom truth and key parameters list object
#'@export
#'
#'@family wrapper functions
#'@author Sarah Gaichas
#'
#'@examples
#'library(here)
#'CC3om <- om_init(here("config/CC3config.r"))
#'
#' # requires these files in config, for example CC3config.R is:
#'
#' # d.name <- here::here("atlantisoutput","CC_2063_OA_OFF_22")
#' # functional.groups.file <- "CalCurrentV3Groups.csv"
#' # biomass.pools.file <- "DIVCalCurrentV3_Biol.nc"
#' # biol.prm.file <- "CalCurrentV3_Biol.prm"
#' # box.file <- "CalCurrentV3_utm.bgm"
#' # initial.conditions.file <- "DIVCalCurrentV3_Biol.nc"
#' # run.prm.file <- "CalCurrentV3_run.xml"
#' # scenario.name <- "CCV3"
#' # bioind.file <- "outputCCV3BiomIndx.txt"
#' # catch.file <- "outputCCV3Catch.txt"

#'
om_init <- function(config = configfile){

  # Where are the atlantis output files? Consider filling with shiny app in future
  source(config)
  # needs these files, for example config file CC3config.R is:

  # d.name <- here::here("atlantisoutput","CC_2063_OA_OFF_22")
  # functional.groups.file <- "CalCurrentV3Groups.csv"
  # biomass.pools.file <- "DIVCalCurrentV3_Biol.nc"
  # biol.prm.file <- "CalCurrentV3_Biol.prm"
  # box.file <- "CalCurrentV3_utm.bgm"
  # initial.conditions.file <- "DIVCalCurrentV3_Biol.nc"
  # run.prm.file <- "CalCurrentV3_run.xml"
  # scenario.name <- "CCV3"
  # bioind.file <- "outputCCV3BiomIndx.txt"
  # catch.file <- "outputCCV3Catch.txt"
  # annage <- FALSE

  #Load functional groups
  funct.groups <- atlantisom::load_fgs(dir=d.name,
                                       file_fgs = functional.groups.file)
  #Get just the names of active functional groups
  funct.group.names <- funct.groups %>%
    filter(IsTurnedOn == 1) %>%
    select(Name) %>%
    .$Name

  # load true total biomass in tons
  truetotbio <- atlantisom::load_bioind(d.name, file_bioind = bioind.file, fgs = funct.groups)

  # load true catch in tons
  truecatchbio <- atlantisom::load_catch(d.name, file_catch = catch.file, fgs = funct.groups)

  # load YOY
  YOY <- atlantisom::load_yoy(d.name, paste0("output", scenario.name, "YOY.txt"))

  # load biol_prm
  biol <- atlantisom::load_biolprm(d.name, biol.prm.file)

  # load run_prm
  runpar <- atlantisom::load_runprm(d.name, run.prm.file)

  # load box info file
  boxpars <- atlantisom::load_box(d.name, box.file)

  # default run_truth setup will save the file, so check for that first
  if(!file.exists(file.path(d.name,
                            paste0(scenario.name, "run_truth.RData")))){
    #Store all loaded results into an R object
    truth <- atlantisom::run_truth(scenario = scenario.name,
                                   dir = d.name,
                                   file_fgs = functional.groups.file,
                                   file_bgm = box.file,
                                   select_groups = funct.group.names,
                                   file_init = initial.conditions.file,
                                   file_biolprm = biol.prm.file,
                                   file_runprm = run.prm.file,
                                   verbose = TRUE,
                                   annage = annage
    )
  } else {
    truth <- get(load(file.path(d.name,
                                paste0("output", scenario.name, "run_truth.RData"))))
  }

  omlist <-list("funct.groups" = funct.groups,
                "funct.group.names" = funct.group.names,
                "truetotbio" = truetotbio,
                "truecatchbio" = truecatchbio,
                "YOY" = YOY,
                "biol" = biol,
                "runpar" = runpar,
                "boxpars" = boxpars,
                "truth" = truth)

  return(omlist)
}
