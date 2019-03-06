#' Load Atlantis scenario output
#'
#' Reads in data generated from an Atlantis scenario and returns a list
#' containing the desired information. The list contains the 'truth' as known
#' from the Atlantis scenario. The truth can later be sampled
#' from to create a data set with observation error.
#' Currently, the \code{run_truth} depends on the following files
#' being in your working directory (\code{dir}):
#' \itemize{
#'  \item{"functionalGroups.csv"}
#'  \item{"[...]TOTCATCH.nc"}
#'  \item{"[...]DietCheck.txt"}
#' },
#' where [...] specifies the entry used for the \code{scenario} argument.
#'
#' @family run functions
#' @author Sean Lucey, Kelli Faye Johnson
#'
#' @template scenario
#' @template dir
#' @template file_fgs
#' @template file_bgm
#' @template select_groups
#' @template file_init
#' @template file_biolprm
#' @template file_runprm
#' @template verbose
#' @template save
#'
#' @return Returns a list object.
#' @export
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' groups <- load_fgs(dir = d, "functionalGroups.csv")
#' run_truth(scenario = "SETAS",
#'   dir = d,
#'   file_fgs = "functionalGroups.csv",
#'   file_bgm = "VMPA_setas.bgm",
#'   select_groups = groups[groups$IsTurnedOn > 0, "Name"],
#'   file_init = "INIT_VMPA_Jan2015.nc",
#'   file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm",
#'   file_runprm = "VMPA_setas_run_fishing_F_Trunk.xml")
#'
run_truth <- function(scenario, dir = getwd(),
  file_fgs, file_bgm, select_groups, file_init, file_biolprm, file_runprm,
  verbose = FALSE, save = TRUE){

  # Read in information
  # Read in the functional groups csv since that is used by many functions
  fgs <- load_fgs(dir = dir, file_fgs = file_fgs)
  # Read in the biomass pools
  bps <- load_bps(dir = dir, fgs = fgs, file_init = file_init)
  # Read in the biological parameters
  biol <- load_biolprm(dir = dir, file_biolprm = file_biolprm)
  # Read in the run parameters
  runprm <- load_runprm(dir = dir, file_runprm = file_runprm)

  nc_catch <- paste0("output", scenario, 'CATCH.nc')
  dietcheck <- paste0("output", scenario, 'DietCheck.txt')
  nc_out <- paste0("output", scenario, ".nc")
  nc_prod <- paste0("output", scenario, "PROD.nc")
  file_catchfish <- file.path(dir,
    paste0("output", scenario, "CatchPerFishery.txt"))
  file_catch <- file.path(dir,
    paste0("output", scenario, "Catch.txt"))

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
  if(verbose) message("Numbers read in.")

  resn <- load_nc(dir = dir,
                  file_nc = nc_out,
                  bps = bps,
                  fgs = fgs,
                  select_groups = select_groups,
                  select_variable = "ResN",
                  check_acronyms = TRUE,
                  bboxes = boxes)
  if(verbose) message("Reserve nitrogen read in.")

  structn <- load_nc(dir = dir,
                  file_nc = nc_out,
                  bps = bps,
                  fgs = fgs,
                  select_groups = select_groups,
                  select_variable = "StructN",
                  check_acronyms = TRUE,
                  bboxes = boxes)
  if(verbose) message("Structural nitrogen read in.")

  eat <- load_nc(dir = dir,
                     file_nc = nc_prod,
                     bps = bps,
                     fgs = fgs,
                     select_groups = select_groups,
                     select_variable = "Eat",
                     check_acronyms = TRUE,
                     bboxes = boxes)
  if(verbose) message("Eaten read in.")

  grazing <- load_nc(dir = dir,
                 file_nc = nc_prod,
                 bps = bps,
                 fgs = fgs,
                 select_groups = select_groups,
                 select_variable = "Grazing",
                 check_acronyms = TRUE,
                 bboxes = boxes)
  if(verbose) message("Grazing read in.")

  vol <- load_nc_physics(dir = dir,
                         file_nc = nc_out,
                         physic_variables = "volume",
                         aggregate_layers = FALSE,
                         bboxes = boxes)
  if(verbose) message("Volume read in.")

  catch <- load_nc(dir = dir,
                 file_nc = nc_catch,
                 bps = bps,
                 fgs = fgs,
                 select_groups = select_groups,
                 select_variable = "Catch",
                 check_acronyms = TRUE,
                 bboxes = boxes)
  if(verbose) message("Catch read in.")

  catchfish <- read.table(file_catchfish, header = TRUE)
  over <- colnames(catchfish)[-(1:2)]
  catchfish <- reshape(catchfish, direction = "long",
    varying = over, v.names = "catch",
    timevar = "species", times = over)
  rownames(catchfish) <- 1:NROW(catchfish)
  catchfish <- catchfish[catchfish$catch > 0,
    -which(colnames(catchfish) == "id")]
  catchfish$species <- fgs$Name[match(catchfish$species, fgs$Code)]
  colnames(catchfish) <- tolower(colnames(catchfish))
  catchfish$time <- catchfish$time / runprm$toutfinc
  if(verbose) message("Catch per fishery read in.")

  # Get catch from txt. Sum per species and compare with values from nc-file!
  catch_all <- read.table(file = file_catch, header = TRUE, sep = " ")
  over <- colnames(catch_all)[(colnames(catch_all) %in% fgs$Code)]
  catch_all <- reshape(catch_all[, c("Time", over)], direction = "long",
    varying = over, v.names = "catch",
    timevar = "species", times = over)
  rownames(catch_all) <- 1:NROW(catch_all)
  catch_all <- catch_all[catch_all$catch > 0,
    -which(colnames(catch_all) == "id")]
  catch_all$species <- fgs$Name[match(catch_all$species, fgs$Code)]
  colnames(catch_all) <- tolower(colnames(catch_all))
  catch_all$time <- catch_all$time / runprm$toutfinc
  if(verbose) message("Catch for all fisheries in biomass read in.")

  diet <- load_diet_comp(dir = dir, file_diet = dietcheck, fgs = fgs,
    toutinc = runprm$toutinc)

  if(verbose) message("Start calc_functions")
  catchbio <- calc_biomass_age(nums = catch,
    resn = resn, structn = structn, biolprm = biol)
  biomass_eaten <- calc_pred_diet(dietcomp = diet,
    eat = eat, grazing = grazing, vol = vol, biolprm = biol)
  biomass_ages <- calc_biomass_age(nums = nums,
    resn = resn, structn = structn, biolprm = biol)
  bio_catch <- calc_biomass_age(nums = catch,
    resn = resn, structn = structn, biolprm = biol)

  bio_catch <- aggregate(atoutput ~ species + time,
    data = bio_catch, sum)

  # todo: check that the biomass of the catches are correct
  # also should catch in biomass be exported as well
  # as catch in numbers?
  check <- merge(catch_all, bio_catch,
    by = c("species", "time"))
  check$check <- with(check, atoutput / catch)

  result <- list("biomass_eaten" = biomass_eaten,
                 "biomass_ages" = biomass_ages,
                 "catch" = catch,
                 "nums" = nums,
                 "resn" = resn,
                 "structn" = structn,
                 "biolprm" = biol,
                 "fgs" = fgs)

  if(verbose) message("Start writing to HDD.")
  if(save) {
    save(result,
      file = file.path(dir, paste0("output", scenario, "run_truth.RData")))
  }

  invisible(result)
}
