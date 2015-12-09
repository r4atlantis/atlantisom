#' A function to sample from the environmental truth provided
#' by the Atlantis scenario.
#'
#' @description Uses data provided by an Atlantis scenario and adds
#'   observation error and/or bias, along with subsetting, to provide
#'   an environmental index for use in subsequent model fitting.
#'
#' @family create functions
#'
#' @param true_env A \code{data.frame} of true environmental data
#'   sampled from an Atlantis scenario.
#' @param subset A \code{data.frame} specifying which samples to keep
#'
#' @author Kelli Faye Johnson
#'
#' @export
#'
create_env <- function(true_env, subset) {
  #
  data <- list()
  invisible(data)
}
