#' Open the necessary files to document the atlantis model used
#' for generating data from the scenario
#'
#' @template dir
#' @template file_out
#'
#' @author Kelli Faye Johnson
#' @return A \code{list} of metadata pertaining to a single Atlantis scenario.
#'
#' @importFrom RNetCDF open.nc close.nc att.get.nc
#'
#' @export
#'
load_meta <- function(dir = getwd(), file_out = "atlantisom_log.txt") {
  data <- list()

  # Main nc file
  grepnc <- "^output[[:alnum:]]+\\.nc"
  file <- dir(dir, pattern = grepnc, full.names = TRUE)[1]
  connection <- open.nc(file)
  on.exit(close.nc(connection))

  # Get model name from the outputMODELNAME.nc file
  data$modelname <- dir(dir, pattern = grepnc)
  data$modelname <- gsub("output|\\.nc", "", data$modelname)
  data$modelname <- data$modelname[which.min(nchar(data$modelname))]

  # Get Atlantis version number
  file.log <- dir(dir, pattern = "^log", full.names = TRUE)
  if (!file.exists(file.log)) {
    warning(paste("The log file does not exist in\n", dir, "\n",
      "and consequently the metadata will list the version number",
      "as NA."))
    data$atlantisversion <- NA
  } else {
    data$atlantisversion <- readLines(file.log, n = 2)[2]
    data$atlantisversion <- gsub("#", "", data$atlantisversion)
  }

  # Get number of years and timestep
  file.prm <- dir(dir, pattern = "\\.prm", full.names = TRUE)
  if (!file.exists(file.prm)) {
    warning(paste("The .prm file does not exist in\n", dir, "\n",
      "and consequently the metadata will list the number of years,",
      "timestep, and timestepunit as NA."))
    data$nyears <- NA
    data$timestep <- NA
    data$timestepunit <- NA
  } else {
    prm <- readLines(file.prm)
    data$nyears <- grep("tstop", prm, value = TRUE)
    data$nyears <- strsplit(data$nyears, "\\t")[[1]][2]
    data$nyears <- strsplit(data$nyears, "[[:space:]]+")[[1]][1]
    data$nyears <- as.numeric(data$nyears) / 365
    data$timestep <- grep("^dt", prm, value = TRUE)
    data$timestep <- strsplit(data$timestep, "\\t")[[1]][2]
    data$timestep <- strsplit(data$timestep, "[[:space:]]+")[[1]]
    data$timestepunit <- data$timestep[2]
    data$timestep <- as.numeric(data$timestep[1])
  }

  # Get box number information
  file.bgm <- dir(dir, pattern = "\\.bgm", full.names = TRUE)
  if (length(file.bgm) < 1) {
    warning(paste("The .bgm file does not exist in\n", dir, "\n",
      "and consequently the metadata will list the number",
      "of boxes and\n maximum depth as NA."))
    data$nbox <- NA
    data$depthmax <- NA
  } else {
    bgm <- readLines(file.bgm)
    data$nbox <- grep("nbox", bgm, value = TRUE)
    data$nbox <- gsub("nbox|[[:space:]]+", "", data$nbox)
    data$nbox <- as.numeric(data$nbox)

    # Get maximum depth
    data$depthmax <- grep("maxwcbotz", bgm, value = TRUE)
    data$depthmax <- gsub("maxwcbotz|[[:space:]]+", "", data$depthmax)
    data$depthmax <- as.numeric(data$depthmax)
  }

  # Get the size of the study area in kilometers^2
  # Assuming that dz (height) of lowest box is 1 m, one can assume that
  # the area is equal to the volume of the lowest box
  volume <- var.get.nc(ncfile = connection, variable = "volume")
  dz <- var.get.nc(ncfile = connection, variable = "dz")
  area <- volume[dim(volume)[1], , ] / dz[dim(dz)[1], 1, 1]
  # If the assumption of the lowest box having a height of 1 m is false you
  # would need to divide by the sum of meters of sediment here
  data$area <- sum(area[2:dim(area)[1], 1]) / 1000000
  data$areaunit <- "km^2"

  # Print the log information to a text file
  file.out <- file.path(dir, file_out)
  sink(file.out)
  cat("# Meta data from atlantisom\n")
  cat(paste("# Written by load_meta on", Sys.time(), "\n"))
  cat(paste("# by", Sys.info()["user"], "on", Sys.info()["sysname"], "\n"))
  cat(paste("# using", version$version.string, "\n"))
  for (x in seq_along(data)) {
    cat(paste0("#", names(data)[x], "\n"))
    cat(data[[x]])
    cat("\n")
  }
  sink()

  invisible(data)
}
