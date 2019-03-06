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
#' runprm <- load_runprm(d, "VMPA_setas_run_fishing_F_Trunk.xml")
#' rm(runprm)
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

  #Read in XML file
  runprm <- xml2::read_xml(file.runprm)

  data <- list()



  #Get nodes with tout names
  node_touts<- as.character(xml2::xml_find_all(runprm,
  "//Attribute[starts-with(@AttributeName,'tout')]"))

  #Get nodes with AttributeName=tstop
  attribute_names <- c("tstop",
                       "dt",
                       "flaghemisphere",
                       "K_num_tot_sp",
                       "fishout",
                       "K_num_fisheries")
  char_tags <- lapply(attribute_names,
                      read_xml_atlantis,
                      xml_input=runprm)

  # Pull the output start, output periodicity, and fisheries periodicity and
  # write to the returned data frame "data"
  time <- sapply(strsplit(node_touts,"="),
    function(x) gsub("[[:punct:]]| Attribute[[:alpha:]]+| day", "", x))
  time <- cbind(time, sapply(strsplit(char_tags[[1]], "="),
    function(x) gsub("[[:punct:]]| Attribute[[:alpha:]]+| day", "", x))[, 1])
  apply(time, 2,
    function(x) eval(parse(text = paste0("data$", x[2], " <<- ", x[4]))))

  data$nyears <- as.numeric(data$tstop) / 365
  data$timestep <- sapply(strsplit(char_tags[[2]], "="),
    function(x) gsub("[[:punct:]]| Attribute[[:alpha:]]+| day", "", x))
  data$timestepunit <- data$timestep[4, 1]
  data$timestep <- as.numeric(data$timestep[7, 1])
  data$outputstep <- data$toutinc
  data$outputstepunit <- "days"
  data$hemisphere <- as.vector(ifelse(sapply(strsplit(char_tags[[3]], "="),
      function(x) gsub("[[:punct:]]| Attribute[[:alpha:]]+| day", "",
      x))[6, 1] == 0, "southern", "northern"))

    data$nspp <- sapply(strsplit(char_tags[[4]],
                                 "="),
    function(x) gsub("[[:punct:]]| Attribute[[:alpha:]]+| day", "", x))[4, 1]
    data$nspp <- as.numeric(data$nspp)

  fishing <- sapply(strsplit(char_tags[[5]], "="),
    function(x) gsub("[[:punct:]]| Attribute[[:alpha:]]+| day", "", x))[6, 1]
    if (fishing == 1) {
        data$nfleet <- sapply(strsplit(char_tags[[6]], "="),
    function(x) gsub("[[:punct:]]| Attribute[[:alpha:]]+| day", "", x))[4, 1]
        data$nfleet <- as.numeric(data$nfleet)
    } else {
      data$nfleet <- 0
    }

  return(data)
}
