% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/create_fishery_subset.R
\name{create_fishery_subset}
\alias{create_fishery_subset}
\title{Create fishery subset from Atlantis output}
\usage{
create_fishery_subset(dat, time, fleets, species, boxes)
}
\arguments{
\item{dat}{A \code{data.frame} of numbers at age containing the following columns:
\itemize{
  \item{species}
  \item{agecl}
  \item{polygon}
  \item{layer}
  \item{time}
  \item{atoutput}
}
The \code{data.frame} is generated from either \code{\link{create_survey}}
or \code{\link{create_fishery_subset}}.}

\item{time}{The timing of the survey (a vector indicating specific time steps, which are typically associated with years)
i.e., seq(365,10*3650,365) would be an annual survey for 10 years}

\item{fleets}{which fleet indices to aggregate in the output (NULL aggregates all fleets)}

\item{species}{The species to sample in the survey (a vector)}

\item{boxes}{A vector of box numbers}
}
\value{
The function returns a subsetted matrix aggregated over fleets with the columns
  as the input data, i.e.,:
  \itemize{
    \item{species}
    \item{agecl}
    \item{polygon}
    \item{layer}
    \item{time}
    \item{atoutput}
  }
This function is for a vector of defined species
Returns only boxes where fishery was sampled
}
\description{
Create a subset of the fishery observations for an Atlantis scenario.
}
\details{
The function takes fishery catch data from an Atlantis scenario
  where the data was read in from Atlantis output using \code{\link{load_nc}}
  within \code{\link{run_truth}}. One does not need to use these functions
  to create \code{dat}, rather you must only ensure that the structure of
  \code{dat} is the same.
  Currently, the function subsets the data by fleet, polygon and time (there
  are not layers in fishery outputs).  The input is fishery observations (either
  catch nums or catch bio outputs of run_truth), so nothing else needs to be done.
  Atlantis already applies a fishery efficiency and selectivity internally.
  This function works for specific defined species, specific defined polygons,
  and specific defined time.
}
\author{
Poseidon
}
