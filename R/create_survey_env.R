#' @title Create environmental survey data subset from Atlantis output

#' @description Create a subset of the physical environment observations for an Atlantis scenario.

#' @details The function takes physical data from an Atlantis scenario
#'   where the data was read in from Atlantis output using \code{\link{load_nc_physics}}.
#'   One does not need to use these functions to create \code{dat}, rather you must only
#'   ensure that the structure of \code{dat} is the same.
#'   Currently, the function subsets the data by polygon and time, with the user defining
#'   a single layer, either "bottom" or "surface", to sample. Possible environmental variables
#'   from \code{\link{load_nc_physics}} include "salt", "NO3", "NH3", "Temp", "Oxygen", "Si",
#'   "Det_Si", "DON", "Chl_a", "Denitrifiction", "Nitrification".
#'   Only one variable is loaded with a call to \code{\link{load_nc_physics}}, so this
#'   function also takes one environmental variable at a time and subsets to specific
#'   defined polygons, and specific defined time representing a survey.
#'
#' @description Uses data provided by an Atlantis scenario and adds
#'   observation error and/or bias, along with subsetting, to provide
#'   an environmental index for use in subsequent model fitting.
#'
#' @family create functions
#'
#' @param true_env A \code{data.frame} of true environmental data
#'   sampled from an Atlantis scenario.
#' @param time   The timing of the survey (a vector indicating specific time steps, which are typically associated with years)
#'                    i.e., seq(365,10*3650,365) would be an annual survey for 10 years
#' @param layer  Which layer to sample, surface or bottom? or numeric model layer
#' @param boxes  A vector of box numbers

#' @return The function returns a subsetted matrix with the same columns
#'   as the input data, i.e.,:
#'   \itemize{
#'     \item{variable}
#'     \item{polygon}
#'     \item{layer}
#'     \item{time}
#'     \item{atoutput}
#'   }
#'
#' @author Kelli Faye Johnson, Sarah Gaichas
#'
#' @importFrom magrittr %>%
#' @export
#'
create_survey_env <- function(true_env,survdepth,survboxes,survtimes) {

  if(survdepth %in% c("bottom", "surface")){
    if(survdepth=="bottom") {
      true_env <- true_env %>%
        dplyr::filter(layer == 0) # according to Atlantis wiki, layer 0 is bottom
    }else{
      #if(survdepth == "surface"){
      true_env <- true_env %>%
        dplyr::filter(layer<7) %>%  #layer 7 is always identical temp to layer 0, may be the added sediment layer?
        dplyr::group_by(polygon, time) %>%
        dplyr::filter(layer==max(layer)) %>%
        dplyr::ungroup()
    }
  }else{
    if(is.numeric(survdepth)){
      true_env <- true_env %>%
        dplyr::filter(layer==survdepth)
    }else{
      print("survdepth must be 'surface', 'bottom' or numeric layerid")
    }
  }

    surveydat <- true_env %>%
      dplyr::filter(polygon %in% survboxes) %>%
      dplyr::filter(time %in% survtimes)

    as.data.frame(surveydat)

    return(surveydat)

}
