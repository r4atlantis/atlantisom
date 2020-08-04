#' Load Atlantis diet composition by box and layer from \code{DetaileDietCheck.txt}
#'
#' Imports the \code{DetaileDietCheck.txt} output file from an Atlantis run
#' and converts the file from wide format to long, where there are five
#' columns: age class, time, diet composition, predator species, and
#' prey species.
#'
#' @family load functions
#' @author Sarah Gaichas
#'
#' @template dir
#' @param file_diet A character value, specifying the file name of the
#'   \code{DetailedDietCheck.txt} output file from Atlantis.
#'   If \code{is.null(dir)}, then \code{file_diet} can be the full file
#'   path or a file in your current working directory, or the \code{file_diet}
#'   will be appended to \code{dir} using \code{file.path}.
#' @template fgs
#'
#'@return Returns a data frame of the data to be exported to the AtlantisOM list
#'  object. The atoutput column is diet proportion and there is an extra column
#'  identifying the prey that makes up that proportion of species (predator)
#'  diet. At present not set up for multiple stocks; function will stop.
#'@export
#'
#' @examples
#' \dontrun{
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' file_diet <- grep("DietCheck", dir(d), value = TRUE)
#' fgs <- load_fgs(dir = d, "Functional_groups.csv")
#' temp <- load__detailed_diet_comp(dir = d, file_diet = file_diet, fgs = fgs)
#' rm(temp)
#' }

load_detailed_diet_comp <- function(dir = getwd(), file_diet, fgs){

  if (is.null(dir)) {
    diet.file <- file_diet
  } else {
    diet.file <- file.path(dir, file_diet)
  }
  diet <- data.table::fread(diet.file, data.table = FALSE)

  # SKG June 2020: changing to the other way around. more new models now,
  # only one diet function to change, and "Predator" seems clearer for users.
  if(length(grep("Group",colnames(diet)))>0)
    colnames(diet) <- gsub("Group","Predator",colnames(diet))

  # Change column order
  diet <- diet[, c("Predator", "Cohort", "Time", "Box", "Layer",
    names(diet)[which(!names(diet) %in% c("Predator", "Cohort", "Time", "Box", "Layer"))])]


  # Convert to tidy dataframe to allow joining/merging with other dataframes.
  diet <- tidyr::gather_(data = diet, key_col = "prey", value_col = "dietcomp",
    #gather_cols = names(diet)[(which(names(diet) == "Updated") + 1):NCOL(diet)])
    gather_cols = names(diet)[(which(names(diet) %in% fgs$Code))])

  names(diet) <- tolower(names(diet))

  diet$prey <- as.character(diet$prey)
  diet$predator <- as.character(diet$predator)

  # Change species acronyms to actual names.
  species_names <- fgs[, c("Name", "Code")]
  diet$species <- species_names$Name[match(diet$predator, species_names$Code)]
  diet$prey <- species_names$Name[match(diet$prey, species_names$Code)]
  #diet <- diet[, -which(colnames(diet) == "predator")]

  # diet outputs are not all divisible by toutinc, leave in days
  diet$time.days <- diet$time #/ toutinc

  #todo: fix cohort to agecl
  diet <- diet %>%
    dplyr::mutate(agecl = cohort + 1)

  # sum over layers to return only 2D box diet
  # only works if in weight, not proportion, need flagdietcheck=1 in runprm
  diet <- diet %>%
    dplyr::select(-cohort) %>%
    dplyr::group_by(predator, agecl, time, polygon, prey) %>%
    dplyr::mutate(boxdiet = sum(dietcomp))


  #does the Update column matter? this assumes it doesn't
  #in SETAS_Example, it doesn't make extra comps

  dietcomp <- data.frame(species = diet$species,
                         agecl = diet$agecl,
                         time.days = diet$time.days,
                         polygon = diet$polygon,
                         layer = NA,
                         atoutput = diet$boxdiet,
                         prey = diet$prey)

  return(dietcomp)
}
