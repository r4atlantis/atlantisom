#' @title Load Atlantis \code{[...]BiomIndx.txt} file
#'
#' @description This function loads the Atlantis BiomIndx.txt output file.
#'   Biomass (total annual tonnes) is only available for species that are
#'   turned on in the \code{functionalGroups.csv} file. Function does not
#'   load relative biomass for these species. Also loads ecosystem
#'   indicators (PelDemRatio, PiscivPlankRatio, DivCount, InfEpiRatio,
#'   BSSslope, HabitCover) available in the BiomInx.txt file.
#' @template dir
#' @template file_bioind
#' @template fgs
#' @template verbose
#' @family load functions
#' @return A data frame of total biomass outputs from the \code{BiomIndx.txt} file,
#' formatted similarly to the outputs of run_truth.
#' @author Sarah Gaichas
#' @export
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file <- "outputSETASBiomIndx.txt"
#' fgs <- load_fgs(dir = d, "functionalGroups.csv")
#' test <- load_bioind(dir = d, file_bioind = file, fgs = fgs)
load_bioind <- function(dir, file_bioind, fgs, verbose = FALSE) {
  file.bioind <- file.path(dir, file_bioind)
  truebio <- read.table(file.bioind, header = TRUE)
  truebio <- truebio[, -grep("Rel", colnames(truebio))]

  groupslookup <- fgs[fgs$IsTurnedOn > 0,]

  names(truebio)[match(groupslookup$Code,names(truebio))] <- groupslookup$Name

  truebio <- truebio %>%
     tidyr::gather(species, truebio, -Time)

  out <- data.frame(species = truebio$species,
                    agecl = NA,
                    polygon = NA,
                    layer = NA,
                    time = truebio$Time,
                    atoutput = truebio$truebio)

  out <- out[order(out$species,out$time,out$polygon,out$agecl),]

  return(out)
}
