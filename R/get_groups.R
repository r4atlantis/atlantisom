#' Collection of similar functions which get specific
#' columns from the Atlantis \code{functionalGroups.csv}
#'
#' This collection of functions takes loaded functional
#' groups, via the \code{\link{load_fgs}} function and creates various
#' character strings of group names or acronym names.
#' @family get functions
#' @return Character string.
#'
#' @details Currently, the following character strings can be created
#' - get_groups extracts the column "Name"
#' - get_age_groups extracts the column "Name". Selects groups with 10 ageclasses.
#' - get_acronym extracts the column "Code"
#' - get_age_acronym extracts the column "Code". Selects groups with 10 ageclasses.
#' - get_nonage_acronym extracts the columne "Code". Only groups with ageclasses different from 10 are selected.
#' - get_fish_acronyms ectracts the column "Code". Only groups with InvertType equal to "FISH" or "SHARK" are selected.
#'
#' @export
#' @rdname get_groups
#' @template fgs
get_groups <- function(fgs){
  result <- fgs$Name
  return(result)
}

#' @export
#' @rdname get_groups
#' @template fgs
get_age_groups <- function(fgs){
  result <- subset(fgs, NumCohorts == 10)$Name
  return(result)
}

#' @export
#' @rdname get_groups
#' @template fgs
get_acronyms <- function(fgs){
  result <- fgs[, names(fgs) == "Code"]
  return(result)
}

#' @export
#' @rdname get_groups
#' @template fgs
get_age_acronyms <- function(fgs){
  result <- subset(fgs, NumCohorts == 10, select = "Code")[,1]
  return(result)
}

#' @export
#' @rdname get_groups
#' @template fgs
get_nonage_acronyms <- function(fgs){
  result <- subset(fgs, NumCohorts != 10, select = "Code")[,1]
  return(result)
}

# S
#' @export
#' @rdname get_groups
#' @template fgs
get_fish_acronyms <- function(fgs){
  # Older models use the column GroupType, newer ones use InvertType.
  supported_columns <- c("InvertType", "GroupType")
  if (!any(is.element(names(fgs), supported_columns))) {
    stop(paste("Column names in fgs do not match any of", supported_columns))
  } else {
    result <- fgs$Code[fgs[, is.element(names(fgs), supported_columns)] %in% c("FISH", "SHARK")]
  }
  return(result)
}
