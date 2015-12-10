#' Converts timestep from Atlantis output to actual time!
#'
#' @template dir
#' @template data
#' @template file_runprm
#' @template modelstart
#'
#' @author Alexander Keth
#' @export
#' @family calc functions
#' @seealso \code{\link{load_fgs}}
#'
#' @return dataframe in tidy format with column time showing the actual time.
#'
#' @examples
# testscenario <- "INIT_VMPA_Jan2015"
# d <- system.file("extdata", testscenario, package = "atlantisom")
# load(file = file.path(d, "outputSETASrun_atlantis.RData"))
# data <- result$biomass_ages
# test <- convert_time(data = data, file_runprm = "test")

convert_time <- function(dir, data, file_runprm, modelstart){
  if (!is.null(dir)) file_runprm <- file.path(dir, file_runprm)

  # Exract timesetp from Parameterfile!
  toutinc <- readLines(con = file_runprm, warn = F)
  toutinc <- toutinc[grep(pattern = "toutinc", x = toutinc)]
  toutinc <- str_split_twice(char = toutinc)
  if (any(names(data) == "time")) {
    years <- trunc(data$time * toutinc / 365) + years(modelstart)
    # use months() to extract months
    # use days() to extract days
    # convert to dates object in dataframe
  }
  return(data)
}
