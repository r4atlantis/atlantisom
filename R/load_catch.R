#' @title Load Atlantis \code{[...]catch.txt} file
#'
#' @description This function loads the Atlantis catch.txt output file.
#'   Where catch (total annual tonnes) is only available for species that are
#'   fished in the \code{functionalGroups.csv} file.
#' @template dir
#' @template file_catch
#' @template fgs
#' @template verbose
#' @family load functions
#' @return A data frame of total catch weight outputs from the \code{catch.txt} file,
#' formatted similarly to the outputs of run_truth.
#' @author Sarah Gaichas
#' @export
#'
#' @examples
#' d <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' file <- "outputSETASCatch.txt"
#' fgs <- load_fgs(dir = d, "functionalGroups.csv")
#' test <- load_catch(dir = d, fgs = fgs, file_catch = file)
load_catch <- function(dir, file_catch, fgs, verbose = FALSE) {
  file.catch <- file.path(dir, file_catch)
  catchbio <- read.table(file.catch, header = TRUE)
  catchbio <- catchbio[, -grep("TsAct", colnames(catchbio))]

  fgs <- fgs[fgs$IsTurnedOn > 0,]
  fishedlookup <- fgs[fgs$IsFished > 0,]

  names(catchbio)[match(fishedlookup$Code,names(catchbio))] <- fishedlookup$Name

  catchbio <- catchbio %>%
     tidyr::gather(species, catchbio, -Time)

  out <- data.frame(species = catchbio$species,
                    agecl = NA,
                    polygon = NA,
                    layer = NA,
                    time = catchbio$Time,
                    atoutput = catchbio$catchbio)

  out <- out[order(out$species,out$time,out$polygon,out$agecl),]

  return(out)
}
