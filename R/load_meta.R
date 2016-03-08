#' Open the necessary files to document the atlantis model used
#' for generating data from the scenario
#'
#' @template dir
#' @template scenario
#' @template verbose
#'
#' @author Kelli Faye Johnson
#' @family load functions
#' @return A \code{list} of metadata pertaining to a single Atlantis scenario.
#' @seealso \code{\link{write_meta}}
#'
#' @export
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' temp <- load_meta(d, "SETAS", verbose = FALSE)
#' rm(temp)
#'
load_meta <- function(dir = getwd(), scenario, verbose = FALSE) {

  # Functions used inside load_meta
  getsplit <- function(get, split = "[[:space:]]+", data) {
    info <- grep(get, data, value = TRUE)
    info <- strsplit(info, split)[[1]][-1]
    return(info)
  }

  if (is.null(dir)) {
    warning(paste("The function load_meta does not allow dir = NULL\n",
      "and dir will be set to\n", getwd(), "\nas determined using getwd()"))
  }
  data <- list()

  # Main nc file
  grepnc <- paste0("^output", scenario, "\\.nc")
  file <- dir(dir, pattern = grepnc, full.names = TRUE)
  connection <- RNetCDF::open.nc(file)
  on.exit(RNetCDF::close.nc(connection))

  # Get model name from the scenario argument
  data$modelname <- scenario

  # Get Atlantis version number
  file.log <- dir(dir, pattern = "^log", full.names = TRUE)
  if (length(file.log) > 1) {
    warning(paste("There are more than one log files in your directory:\n",
      paste(file.log, collapse = "\n"),
      "and only the last one, i.e., the one with the largest sequence number",
      "will be used.\n",
      "load_meta will use the following log file:\n", tail(file.log, 1)))
    file.log <- tail(file.log, 1)
  }
  if (!file.exists(file.log)) {
    warning(paste("The log file does not exist in\n", dir, "\n",
      "and consequently the metadata will list the version number",
      "as NA."))
    data$atlantisversion <- NA
  } else {
    data$atlantisversion <- readLines(file.log, n = 2)[2]
    data$atlantisversion <- gsub("#", "", data$atlantisversion)
  }

  # Get number of years, timestep, output interval,
  # number of species, and number of fleets
  file.prm <- dir(dir, pattern = "\\.xml", full.names = TRUE)
  if (length(file.prm) > 1) {
    temp <- dir(dir, pattern = "run\\.xml", full.names = TRUE)
    if (length(temp) == 0) {
      file.prm <- dir(dir, pattern = "\\.xml", full.names = TRUE)[1]
      warning(paste("There are more than one xml files in your directory:\n",
      "and only the last one, i.e., the one with the largest sequence number",
      "will be used.\n",
      "load_meta will use the following prm file:\n", file.prm))
    } else {
      file.prm <- temp
      rm(temp)
    }
  }
  if (length(file.prm) == 0) {
    warning(paste("The .prm file does not exist in\n", dir, "\n",
      "and consequently the metadata will list the number of years,",
      "timestep, and timestepunit as NA."))
    data$toutstart <- NA
    data$toutinc <- NA
    data$toutfinc <- NA
    data$tstop <- NA
    data$nyears <- NA
    data$timestep <- NA
    data$timestepunit <- NA
    data$outputstep <- NA
    data$outputstepunit <- NA
    data$hemisphere <- NA
    data$nspp <- NA
    data$nfleet <- NA
  } else { # Only go here if the prm file is available
    data <- append(data,
      load_runprm(dir = NULL, file.prm))
  } # Close if for prm file

  # Get box number information
  file.bgm <- dir(dir, pattern = "\\.bgm", full.names = TRUE)
  if (length(file.bgm) > 1) {
    stop(paste(file.bgm, collapse = ", "), "were found in", dir,
      "load_meta needs to be fixed to deal with this.")
  }
  if (length(file.bgm) < 1) {
    warning(paste("The .bgm file does not exist in\n", dir, "\n",
      "and consequently the metadata will list the number",
      "of boxes and\n maximum depth as NA."))
    data$nbox <- NA
    data$depthmax <- NA
  } else { # Only go here if the bgm file is available
    bgm <- readLines(file.bgm)
    data$nbox <- grep("nbox", bgm, value = TRUE)
    data$nbox <- gsub("nbox|[[:space:]]+", "", data$nbox)
    data$nbox <- as.numeric(data$nbox)

    # Get maximum depth
    data$depthmax <- grep("maxwcbotz", bgm, value = TRUE)
    data$depthmax <- gsub("maxwcbotz|[[:space:]]+", "", data$depthmax)
    data$depthmax <- as.numeric(data$depthmax)
  } # Close if for bgm file

  # Get the size of the study area in kilometers^2
  # Assuming that dz (height) of lowest box is 1 m, one can assume that
  # the area is equal to the volume of the lowest box
  volume <- RNetCDF::var.get.nc(ncfile = connection, variable = "volume")
  dz <- RNetCDF::var.get.nc(ncfile = connection, variable = "dz")
  area <- volume[dim(volume)[1], , ] / dz[dim(dz)[1], 1, 1]
  # If the assumption of the lowest box having a height of 1 m is false you
  # would need to divide by the sum of meters of sediment here
  data$area <- sum(area[2:dim(area)[1], 1]) / 1000000
  data$areaunit <- "km^2"

  invisible(data)
}
