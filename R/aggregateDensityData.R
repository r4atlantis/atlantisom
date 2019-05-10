#' @title Aggregate and select density data from Atlantis output
#'
#' @description subsets data based on defined columns, and takes median over
#' layers, but enter \code{NA} for layers to
#'   keep the column names the same.
#'   Currently, the function works for a vector of defined species and polygons.
#' @author Sarah Gaichas, Allan Hicks
#' @export
#'
#' @param dat A \code{data.frame} of density data (structn, resn) from reading
#'   in the Atlantis output
#' @param time A single value or a \code{vector} of time steps for
#'   a survey. Time steps are specified as integers.
#' @param species A \code{vector} of character values, where each
#'   entry is a species name that is sampled in the survey.
#' @param boxes A \code{vector} of polygons where the survey samples
#'   within each specified polygon.
#' @param keepColumns A \code{vector} of character values, specifying
#'   which columns to keep for the returned \code{data.frame}.
#'   The default is \code{c("species", "agecl", "polygon", "time")}.
#'
#' @return The function returns a subsetted matrix with the same columns
#'   as the input data, i.e.,:
#'   \itemize{
#'     \item{species}
#'     \item{agecl}
#'     \item{polygon}
#'     \item{layer}
#'     \item{time}
#'     \item{atoutput}
#'   }
#'
#' @examples
#' dat <- data.frame(species = c(rep("spec1", 3*3),rep("spec2", 5*3)),
#'   agecl = c(rep(1:3, 3), rep(3:7, 3)),
#'   polygon = c(rep(1:3, each = 3), rep(1:3, each = 5)),
#'   layer = 1, time = 1)
#' dat$atoutput <- 10000/dat$agecl
#' tmp <- aggregateData(dat = dat, time = 1,
#'   species = c("spec1", "spec2"), boxes = 1:2)
#' rm(list = c("tmp", "dat"))
#'
aggregateData <- function(dat, time, species, boxes,
  keepColumns = c("species", "agecl", "polygon", "time")) {

  #first select the appropriate rows (time and box)
  sampDat <- dat[dat$time%in%time &
                 dat$polygon%in%boxes &
                 dat$species%in%species, ]

  #sum over a variable
  aggDat <- aggregate(sampDat$atoutput, as.list(sampDat[, c(keepColumns)]), median)
  names(aggDat) <- c(keepColumns, "medatoutput")

  return(aggDat)
}

