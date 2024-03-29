% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/load_agebioind.R
\name{load_agebioind}
\alias{load_agebioind}
\title{Load Atlantis \code{[...]AgeBiomIndx.txt} file}
\usage{
load_agebioind(dir, file_agebioind, fgs, verbose = FALSE)
}
\arguments{
\item{dir}{The directory of the atlantis model output, where the
default is \code{getwd()}.}

\item{file_agebioind}{A character value specifying the name of the output text file
that contains total annual biomass by age (in tonnes) for an Atlantis scenario.
An example entry would be \code{"outputSETASAgeBiomIndx.txt"}.}

\item{fgs}{A data frame created by \code{\link{load_fgs}} that
reads in the csv file containing functional group names, usually
\code{"functionalGroups.csv"}.}

\item{verbose}{A logical value specifying if output should be written
to the screen or not. The default value is \code{FALSE}.}
}
\value{
A data frame of total biomass outputs from the \code{AgeBiomIndx.txt} file,
formatted similarly to the outputs of run_truth.
}
\description{
This function loads the Atlantis AgeBiomIndx.txt output file.
  Biomass (total annual tonnes) is only available for species that are
  turned on in the \code{functionalGroups.csv} file. Function does not
  load relative biomass for these species.
}
\examples{
\dontrun{
d <- system.file("extdata", "SETAS_Example", package = "atlantisom")
file <- "outputsAgeBiomIndx.txt"
fgs <- load_fgs(dir = d, "Functional_groups.csv")
test <- load_agebioind(dir = d, file_bioind = file, fgs = fgs)
}
}
\seealso{
Other load functions: 
\code{\link{load_bioind}()},
\code{\link{load_biolprm}()},
\code{\link{load_boxarea}()},
\code{\link{load_box}()},
\code{\link{load_bps}()},
\code{\link{load_catch}()},
\code{\link{load_detailed_diet_comp}()},
\code{\link{load_diet_comp}()},
\code{\link{load_fgs}()},
\code{\link{load_fisheries}()},
\code{\link{load_meta}()},
\code{\link{load_nc_annage}()},
\code{\link{load_nc_catchtons}()},
\code{\link{load_nc}()},
\code{\link{load_runprm}()},
\code{\link{load_yoy}()}
}
\author{
Sarah Gaichas
}
\concept{load functions}
