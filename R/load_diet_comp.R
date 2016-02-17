#' Load Atlantis diet composition data from \code{diet_check.txt}
#'
#' Imports the \code{diet_check.txt} output file from an Atlantis run
#' and converts the file from wide format to long, where there are five
#' columns: age class, time, diet composition, predator species, and
#' prey species.
#'
#' @family load functions
#' @author Kelli Faye Johnson
#'
#' @template dir
#' @param file_diet A character value, specifying the file name of the
#'   \code{diet_check.txt} output file from Atlantis.
#'   If \code{is.null(dir)}, then \code{file_diet} can be the full file
#'   path or a file in your current working directory, or the \code{file_diet}
#'   will be appended to \code{dir} using \code{file.path}.
#' @template fgs
#'
#'@return Returns a data frame of the data to be exported to the AtlantisOM list
#'  object.
#'@export
#'
#' @examples
#' dir <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file_diet <- grep("DietCheck", dir(dir), value = TRUE)
#' fgs <- load_fgs(dir = dir, "functionalGroups.csv")
#' temp <- load_diet_comp(dir = dir, file_diet = file_diet, fgs = fgs)
#' rm(temp)
#'

load_diet_comp <- function(dir = getwd(), file_diet, fgs){

  if (is.null(dir)) {
    diet.file <- file_diet
  } else {
    diet.file <- file.path(dir, file_diet)
  }
  diet <- read.table(diet.file, header = TRUE)

  # remove unnessesary columns and add ones that aren't present in the data
  # Adding polygon and layer results in serious isses when we combine this dataset
  # with other datasets which have this information. Therefore those columns
  # cannot be added as NA columns! We should also keep in mind that hardcoding
  # the removal of the column stock may result in bugs when models actually
  # work woth multiple stocks (the CaCu model only has 1 stock per functional group).
  if (length(unique(diet$Stock)) > 1) {
    table(diet$Group, diet$Stock)
    stop(paste("There is more than one stock for a functional group\n",
      "in your diet data, and load_diet_comp only works with one stock\n",
      "per functional group:\n"))
  } else {
    diet <- diet[, -which(colnames(diet) == "Stock")]
  }

  # Change column order
  diet <- diet[, c("Group", "Cohort", "Time",
    names(diet)[which(!names(diet) %in% c("Group", "Cohort", "Time"))])]

  # Convert to tidy dataframe to allow joining/merging with other dataframes.
  diet <- tidyr::gather_(data = diet, key_col = "prey", value_col = "dietcomp",
                         gather_cols = names(diet)[(which(names(diet) == "Time") + 1):length(names(diet))])

  names(diet) <- tolower(names(diet))

  diet$prey <- as.character(diet$prey)
  diet$group <- as.character(diet$group)

  # Change species acronyms to actual names.
  species_names <- fgs[, c("Name", "Code")]
  diet <- dplyr::left_join(diet, species_names, by = c("group" = "Code"))
  names(diet)[names(diet) == "Name"] <- "species"

  diet <- dplyr::left_join(diet, species_names, by = c("prey" = "Code"))
  names(diet)[names(diet) == "Name"] <- "prey"

  # Convert days to timesteps
  # todo: use toutinc to get this value
  # NOTE: This is hardcoded as we do not have access to the run.prm file at
  # the moment. The timestep used in the model-run has be extracted from this
  # file to make sure the code is running with other models.
  diet$time <- diet$time / 365

  return(diet)
}
