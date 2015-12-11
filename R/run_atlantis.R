#' Load Atlantis scenario output
#'
#' Reads in data generated from an Atlantis scenario and returns a list
#' containing the desired information. The list contains the 'truth' as known
#' from the Atlantis scenario. The truth can later be sampled
#' from to create a data set with observation error.
#' Currently, the \code{run_atlantis} depends on the following files
#' being in your working directory:
#' \itemize{
#'  \item{"functionalGroups.csv"}
#'  \item{"[...]TOTCATCH.nc"}
#'  \item{"[...]DietCheck.txt"}
#' },
#' where [...] specifies the entry used for the \code{scenario} argument.
#'
#' @family run functions
#' @author Sean Lucey
#'
#' @template scenario
#' @template dir
#' @template file_fgs
#' @template file_bgm
#' @template select_groups
#' @template file_init
#'
#' @return Returns a list object.
#' @export
#' @examples
#'#load_all()
# scenario <- "SETAS"
# run_atlantis(scenario = scenario,
#   dir = system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom"),
#   file_fgs = "functionalGroups.csv",
#   file_bgm = "VMPA_setas.bgm",
#   select_groups = "Planktiv_S_Fish",
#   file_init = "INIT_VMPA_Jan2015.nc",
#   file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")

run_atlantis <- function(scenario, dir = getwd(),
  file_fgs, file_bgm, select_groups, file_init, file_biolprm){

  # Create file names
  if (is.null(dir)) {
    file.fgs <- file_fgs
  } else {
    file.fgs <- file.path(dir, file_fgs)
  }

  # Read in information
  # Read in the functional groups csv since that is used by many functions
  fgs <- load_fgs(dir = dir, file_fgs = file_fgs)
  # Read in the biomass pools
  bps <- load_bps(dir = dir, fgs = fgs, file_init = file_init)
  # Read in the biological parameters
  biol <- load_biolprm(dir = dir, file_biolprm = file_biolprm)

  nc_catch <- paste0("output", scenario, 'CATCH.nc')
  dietcheck <- paste0("output", scenario, 'DietCheck.txt')
  nc_out <- paste0("output", scenario, ".nc")
  nc_prod <- paste0("output", scenario, "PROD.nc")
  # Get the boundary boxes
  allboxes <- load_box(dir = dir, file_bgm = file_bgm)
  boxes <- get_boundary(allboxes)

  #Extract from NetCDF files
  # Need: dir, file_nc, bps, fgs, select_groups, select_variable,
  # check_acronyms, bboxes
  nums <- load_nc(dir = dir,
                  file_nc = nc_out,
                  bps = bps,
                  fgs = fgs,
                  select_groups = select_groups,
                  select_variable = "Nums",
                  check_acronyms = TRUE,
                  bboxes = boxes)
  print("Numbers read in.")

  resn <- load_nc(dir = dir,
                  file_nc = nc_out,
                  bps = bps,
                  fgs = fgs,
                  select_groups = select_groups,
                  select_variable = "ResN",
                  check_acronyms = TRUE,
                  bboxes = boxes)
  print("Reserve nitrogen read in.")

  structn <- load_nc(dir = dir,
                  file_nc = nc_out,
                  bps = bps,
                  fgs = fgs,
                  select_groups = select_groups,
                  select_variable = "StructN",
                  check_acronyms = TRUE,
                  bboxes = boxes)
  print("Structural nitrogen read in.")

  eat <- load_nc(dir = dir,
                     file_nc = nc_prod,
                     bps = bps,
                     fgs = fgs,
                     select_groups = select_groups,
                     select_variable = "Eat",
                     check_acronyms = TRUE,
                     bboxes = boxes)

  grazing <- load_nc(dir = dir,
                 file_nc = nc_prod,
                 bps = bps,
                 fgs = fgs,
                 select_groups = select_groups,
                 select_variable = "Grazing",
                 check_acronyms = TRUE,
                 bboxes = boxes)

  vol <- load_nc_physics(dir = dir,
                         file_nc = nc_out,
                         physic_variables = "volume",
                         aggregate_layers = FALSE,
                         bboxes = boxes)

  catch <- load_nc(dir = dir,
                 file_nc = nc_catch,
                 bps = bps,
                 fgs = fgs,
                 select_groups = select_groups,
                 select_variable = "Catch",
                 check_acronyms = TRUE,
                 bboxes = boxes)
  print("catch read in.")

  diet <- load_diet_comp(dir = dir, dietfile = dietcheck, fgs = fgs)

  print("***Start calc_functions")
  biomass_eaten <- calc_pred_diet(dietcomp = diet, eat = eat, grazing = grazing, vol = vol, biolprm = biol)

  biomass_ages <- calc_biomass_age(nums = nums, resn = resn, structn = structn, biolprm = biol)

  bio_catch <- calc_biomass_age(nums = catch, resn = resn, structn = structn, biolprm = biol)

  result <- list("biomass_eaten" = biomass_eaten,
                 "biomass_ages" = biomass_ages,
                 "catch" = catch,
                 "nums" = nums)

  print("***Start writing to HDD.")
  save(result, file = file.path(dir, paste0("output", scenario, "run_atlantis.RData")))

  print("Hurray, done!")
  return(result)
}

# dir <- file.path("C:", "CaliforniaCurrent", "PoseidonAdventureCalCurrentINPUTdraft1", "CalCurrent65YearsCurrentFishingRates")
# file_fgs <- "CalCurrentV3Groups.csv"
# scenario <- "CCV3"
# file_bgm <- "CalCurrentV3_utm.bgm"
# file_init <- "DIVCalCurrentV3_Biol.nc"
# select_groups <- load_groups(fgs)

# test <- run_atlantis(scenario = "CCV3",
#                      dir = dir,
#                      file_fgs = "CalCurrentV3Groups.csv",
#                      file_bgm = "CalCurrentV3_utm.bgm",
#                      select_groups = load_groups(fgs = read.table(file.path(dir, "CalCurrentV3Groups.csv"), sep = ",", header = T, stringsAsFactors = F)),
#                      file_init = "DIVCalCurrentV3_Biol.nc")


