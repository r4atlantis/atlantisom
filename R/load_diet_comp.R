#'Load Atlantis Diet Composition
#'
#'Uses the diet_check.txt output from Atlantis to be used in the Atlantis
#'Operating model.
#'
#'@family load functions
#'
#' @template dir
#'@param dietfile file name of the diet_check.txt output from Atlantis.
#' @template fgs
#'
#'@return Returns a data frame of the data to be exported to the AtlantisOM list
#'  object.
#'@export
load_diet_comp <- function(dir = getwd(), dietfile, fgs){

  if (is.null(dir)) {
    diet.file <- dietfile
  } else {
    diet.file <- file.path(dir, dietfile)
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
  # NOTE: This is hardcoded as we do not have access to the run.prm file at
  # the moment. The timestep used in the model-run has be extracted from this
  # file to make sure the code is running with other models.
  diet$time <- diet$time / 365

  return(diet)
}
