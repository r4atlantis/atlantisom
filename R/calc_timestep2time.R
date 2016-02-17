#' Converts timestep from Atlantis output to real time
#'
#' @details Atlantis output uses time steps, where time steps are sequential
#'   and start at zero. \code{convert_time} converts the time step to dates,
#'   where zero is January 01, 1970.
#'
#' @template dir
#' @param data A data frame of Atlantis output. Where the data frame can be
#'   of any structure as long as it has a column with the name \code{"time"}.
#' @template file_runprm
#'
#' @author Alexander Keth
#' @export
#' @family calc functions
#' @seealso \code{\link{load_fgs}}
#'
#' @return A \code{data.frame} in tidy format with a column date showing
#'   the real date and year showing the real year.
#'   If there is not column \code{"time"} in the \code{data.frame} specified
#'   for the \code{data}, then the original \code{data.frame}
#'   is returned.
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' runprm <-
#' load(file = file.path(d, "outputSETASrun_truth.RData"))
#' test <- convert_time(dir = d, data = result$biomass_ages,
#'   file_runprm = "VMPA_setas_run_fishing_F_Trunk.xml")
#' rm(test)
#'
convert_time <- function(dir = getwd(), data, file_runprm){
  if (!is.null(dir)) file_runprm <- file.path(dir, file_runprm)

  # Exract timestep from Parameterfile!
  info <- load_runprm(dir = NULL, file_runprm)
  if (any(names(data) == "time")) {
    data$year <- chron::years(data$time * info$toutinc +
      info$toutstart)
    data$date <- chron::chron(data$time * info$toutinc +
          info$toutstart, format = "m-d-Y")
  }
  return(data)
}
