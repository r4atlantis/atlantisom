#' @title Load Atlantis \code{[...]AgeBiomIndx.txt} file
#'
#' @description This function loads the Atlantis AgeBiomIndx.txt output file.
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
#' @return A data frame of total biomass outputs from the \code{AgeBiomIndx.txt} file,
#' formatted similarly to the outputs of run_truth.
#' @author Sarah Gaichas
#' @export
#'
#' @examples
#' \dontrun {
#' d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
#' file <- "outputsAgeBiomIndx.txt"
#' fgs <- load_fgs(dir = d, "Functional_groups.csv")
#' test <- load_agebioind(dir = d, file_bioind = file, fgs = fgs)
#' }
load_agebioind <- function(dir, file_bioind, fgs, verbose = FALSE) {
  file.bioind <- file.path(dir, file_bioind)
  truebio <- read.table(file.bioind, header = TRUE)

  truebio <- truebio %>%
    tidyr::pivot_longer(.,cols=contains("."),names_to = "code",values_to = "atoutput") %>%
    dplyr::mutate(age = as.numeric(unlist(stringr::str_split(code,"\\."))[2])) %>%
    dplyr::mutate(code = unlist(stringr::str_split(code,"\\."))[1])

  groupslookup <- fgs[fgs$IsTurnedOn > 0,] %>%
    dplyr::select(Code,Name)

  out <- truebio %>% dplyr::left_join(.,groupslookup,by = c("code" = "Code")) %>%
    dplyr::rename(species = Name, time=Time) %>%
    dplyr::mutate(agecl=NA,polygon=NA,layer=NA) %>%
    dplyr::select(species,agecl,polygon,layer,time,atoutput,code,age)


  out <- out[order(out$species,out$time,out$age,out$polygon,out$agecl),]

  return(out)
}
