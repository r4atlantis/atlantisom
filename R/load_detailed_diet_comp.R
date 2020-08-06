#' Load Atlantis diet composition by box and layer from \code{DetaileDietCheck.txt}
#'
#' Imports the \code{DetaileDietCheck.txt} output file from an Atlantis run
#' and converts the file from wide format to long, where there are seven
#' columns: (predator) species, age class, time in days, polygon, layer,
#' diet composition in tons (assuming flagdietcheck=1), and prey (species).
#' Only non-zero values for diet compostion are included.
#'
#' @family load functions
#' @author Sarah Gaichas
#'
#' @template dir
#' @param file_diet A character value, specifying the file name of the
#'   \code{DetailedDietCheck.txt} output file from Atlantis.
#'   This file is read with \code{data.table::fread()}, so it can be
#'   compressed with a .gz suffix, or uncompressed as a .txt file.
#'   If \code{is.null(dir)}, then \code{file_diet} can be the full file
#'   path or a file in your current working directory, or the \code{file_diet}
#'   will be appended to \code{dir} using \code{file.path}.
#' @template fgs
#'
#'@return Returns a data frame of the data to be exported to the AtlantisOM list
#'  object. The atoutput column is diet in tons and there is an extra column
#'  identifying the prey that makes up that proportion of species (predator)
#'  diet.
#'@export
#'
#' @examples
#' \dontrun{
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' file_diet <- grep("DetailedDietCheck", dir(d), value = TRUE)
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

  #remove rows that are all 0 prey
  #diet <- diet[apply(diet[,-c(1:5)], 1, function(x) !all(x==0)),]

  #should do the same thing faster
  #diet <- diet[as.logical(abs(as.matrix(diet[,-c(1:5)])) %*% rep(1L,ncol(diet[,-c(1:5)]))), ]

  # slightly slower but maybe less memory intensive?
  diet <- diet[as.logical(rowSums(diet[,-c(1:5)] != 0)), ]

  # Convert to tidy dataframe to allow joining/merging with other dataframes.
  diet <- tidyr::gather_(data = diet, key_col = "prey", value_col = "dietcomp",
    #gather_cols = names(diet)[(which(names(diet) == "Updated") + 1):NCOL(diet)])
    gather_cols = names(diet)[(which(names(diet) %in% fgs$Code))])

  # Get rid of 0 dietcomp rows to make manageable size
  diet <- filter(diet, dietcomp>0)

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

  #fix cohort to agecl
  diet <- diet %>%
    dplyr::mutate(agecl = cohort + 1)

  # drop redundant columns and reorder
  diet <- diet[,c("species", "agecl", "time.days", "box", "layer", "prey", "dietcomp")]

  #leave layer info for now, this is a load function

  #does the Update column matter? this assumes it doesn't
  #in SETAS_Example, it doesn't make extra comps

  dietcompwt <- data.frame(species = diet$species,
                         agecl = diet$agecl,
                         time.days = diet$time.days,
                         polygon = diet$box,
                         layer = diet$layer,
                         atoutput = diet$dietcomp,
                         prey = diet$prey)

  return(dietcompwt)
}
