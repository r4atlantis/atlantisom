#' Extract the box area from the bgm-file!
#'
#'
#' This function extracts the box area for each polygon from the atlantis bgm-file.
#' @template dir
#' @template file_bgm
#' @return dataframe with the columns polygon and area. Polygon gives the box id as integer and area
#' is the box size in m².
#'
#' @family load functions
#' @export

load_boxarea <- function(dir = getwd(), file_bgm) {
  if (is.null(dir)) {
    file.bgm <- file_bgm
  } else {
    file.bgm <- file.path(dir, file_bgm)
  }
  boxarea <- readLines(con = file_bgm, warn = F)

  nr_boxes <- boxarea[grep(pattern = "nbox", x = boxarea)]
  nr_boxes <- str_split_twice(char = nr_boxes)

  search_strings <- paste0("box", 0:(nr_boxes - 1), ".area")

  boxarea <- boxarea[sapply(search_strings, grep, x = boxarea)]
  boxarea <- sapply(boxarea, str_split_twice, USE.NAMES = F)

  boxarea <- data.frame(polygon = 0:(nr_boxes - 1), area = boxarea)

  return(boxarea)
}

# Helper function which can be used every time the value of any flag
# shall be extracted from any atlantis file.
str_split_twice <- function(char){
  # These are the most common patterns used to seperate flags from values.
  # Unfortunately, there is no default seperator for different atlantis models
  # (or within a single model). Therefore we use multiple seperators.
  patterns <- c(" ", "\t")
  if (all(!stringr::str_detect(string = char, pattern = patterns))) {
    stop("Neither space nor tab present in variable char (string)!")
  }
  for (i in seq_along(patterns)) {
    char <- unlist(stringr::str_split(string = char, pattern = patterns[i]))
  }
  char <- suppressWarnings(as.numeric(char))
  if (all(is.na(char))) stop("No numeric value in variable char (string)!")
  char <- char[min(which(!is.na(char)))]
  return(char)
}


