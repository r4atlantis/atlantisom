#' Load Atlantis \code{_run.prm} file
#'
#' This function loads the Atlantis run parameter output file.
#' @template dir
#' @template file_runprm
#' @family load functions
#' @return A list of parameters dictating run characteristics from the
#' \code{_run.xml} file.
#' @author Emma E Hodgson, Kelli Faye Johnson
#' @export
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' load_runprm(d, "VMPA_setas_run_fishing_F_Trunk.xml")
#'
load_runprm <- function(dir = getwd(), file_runprm) {
  if (is.null(dir)) {
    file.runprm <- file_runprm
  } else {
    file.runprm <- file.path(dir, file_runprm)
  }

  # Check that the file has the extension .xml
  extension <- tail(strsplit(file_runprm, "\\.")[[1]], 1)
  if (extension != "xml") {
    stop(paste("The filename specified for load_runprm should end in xml\n",
      "instead the file you specified ends in", extension, "\n",
      "please specify the correct file type."))
  }

  runprm <- readLines(file.runprm)
  data <- list()

  # Pull the output start, output periodicity, and fisheries periodicity and
  # write to the returned data frame "data"
  time <- sapply(strsplit(runprm[grep("tout", runprm)], "="),
    function(x) gsub("[[:punct:]]| Attribute[[:alpha:]]+| day", "", x))
  apply(time, 2,
    function(x) eval(parse(text = paste0("data$", x[2], " <<- ", x[4]))))

  return(data)
}
