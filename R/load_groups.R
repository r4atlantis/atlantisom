#' Collection of similar functions which load specific columns from the Atlantis functionalgroups-file
#'
#'
#' This collection of function loads the Atlantis functional groups file and creates various
#' character strings of group names or acronym names.
#' @param file_fgs Connection of the ATLANTIS functional groups file given as complete folder/filename string.
#' Usually "functionalGroups.csv".
#' @param nc_init Connection of the ATLANTIS init file given as complete folder/filename string. Usually "init[...].nc".
#' @family load functions
#' @return Character string.

#' @details Currently, the following character strings can be created
#' - load_groups extracts the column "Name"
#' - load_age_groups extracts the column "Name". Only groups with 10 ageclasses are selected.
#' - load_acronym extracts the column "Code"
#' - load_age_acronym extracts the column "Code". Only groups with 10 ageclasses are selected.
#' - load_nonage_acronym extracts the columne "Code". Only groups with ageclasses different from 10 are selected.
#' - load_fish_acronyms ectracts the column "Code". Only groups with InvertType equal to "FISH" or "SHARK" are selected.
#' - load_bps extracts the names of the epibenthic biomasspools from the initial conditions file.
#' @keywords gen
#' @examples
#' load_atlantis_output(model_path = file.path("z:", "Atlantis", "ATLANTIS NSmodel base"), filename = "outputNorthSea.nc", select_groups = get_groups(), select_variable = "ResN", biomasspools = c("large_crabs", "small_epifauna", "sessile_epifauna", "epifaunal_macrobenthos"))
#' @export


#' @export
#' @rdname load_groups
load_groups <- function(file_fgs){
  result <- read_functionalgroups(file_fgs = file_fgs)
  result <- result$Name
  return(result)
}

#' @export
#' @rdname load_groups
read_functionalgroups <- function(file_fgs){
  result <- read.table(file = file_fgs, sep = ",", header = T, stringsAsFactors = F)
  return(result)
}


#' @export
#' @rdname load_groups
load_age_groups <- function(file_fgs){
  result <- read_functionalgroups(file_fgs = file_fgs)
  result <- subset(result, NumCohorts == 10)$Name
  return(result)
}

#' @export
#' @rdname load_groups
load_acronyms <- function(file_fgs){
  result <- read_functionalgroups(file_fgs = file_fgs)
  result <- result[, names(result) == "Code"]
  return(result)
}

#' @export
#' @rdname load_groups
load_age_acronyms <- function(file_fgs){
  result <- read_functionalgroups(file_fgs = file_fgs)
  result <- subset(result, NumCohorts == 10, select = "Code")[,1]
  return(result)
}

#' @export
#' @rdname load_groups
load_nonage_acronyms <- function(file_fgs){
  result <- read_functionalgroups(file_fgs = file_fgs)
  result <- subset(result, NumCohorts != 10, select = "Code")[,1]
  return(result)
}

# S
#' @export
#' @rdname load_groups
load_fish_acronyms <- function(file_fgs){
  result <- read_functionalgroups(file_fgs = file_fgs)
  # Older models use the column GroupType, newer ones use InvertType.
  supported_columns <- c("InvertType", "GroupType")
  if (!any(is.element(names(result), supported_columns))) {
    stop(paste("Column names in", file_fgs, "do not match any of", supported_columns))
  } else {
    result <- result$Code[result[, is.element(names(result), supported_columns)] %in% c("FISH", "SHARK")]
  }
  return(result)
}

#' This function still need to load in the initial conditions file to check the number of
#' dimension in the "N" variable. Models with an updated version of the functional-groups
#' file will be able to load the "biomasspools" (which aren't true biomasspools but all epibenthic
#' groups) from the functional groups file directly. This will save considerable amount
#' of time. In addition parameter handling will be much easier, as there is no need to pass
#' the init-file to the function call anymore.
#' @export
#' @rdname helper_functions
load_bps <- function(file_fgs, nc_init){
  init <- RNetCDF::open.nc(con = nc_init)
  all_groups <- load_groups(file_fgs = file_fgs)
  init_vars <- sapply(seq_len(RNetCDF::file.inq.nc(init)$nvars - 1),
                      function(x) RNetCDF::var.inq.nc(init, x)$name)

  search_string <- paste(all_groups, "N", sep = "_")
  search_string <- search_string[is.element(search_string, init_vars)]

  groups <- substr(x = search_string, start = 1, stop = nchar(search_string) - 2)

  bps_id <- vector()
  for (i in seq_along(search_string)){
    bps_id[i] <- RNetCDF::var.inq.nc(ncfile = init, variable = search_string[i])$ndims
  }
  bps_id <- which(bps_id == 2)

  bps <- groups[bps_id]
  return(bps)
}


