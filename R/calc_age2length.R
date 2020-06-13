#' Calculate length from ages
#'
#' Calculate length composition in 1 cm bins from Atlantis output (st)age data
#' Uses numbers at age, plus weight at age, and weight-length relationships to
#' generate size comps
#'
#' @author Gavin Fay
#' @export
#' @template structn
#' @template resn
#' @template nums
#' @template biolprm
#' @template fgs
#' @param maxbin The upper length (cm) bin applied to all species sampled. The default
#' value is 150.
#' @param CVlenage The variability in length at age (currently same for all species)
#' The default value is 0.1.
#' @param remove.zeroes  Logical asking whether to only return numbers at length for
#' combinations of species, age, box, depth, etc that have numbers>0.
#' The default value is \code{TRUE}.
#' @return A \code{list} containing three \code{data.frame}s, mulen (mean length at age),
#' muweight (mean weight at age), and natlength (numbers at length). natlength is in the
#' same format as other dataframes in the atlantisom package except has two additional
#' columns to include the length bin information.
#'
#' @examples
#' \dontrun{
#' directory <- system.file("extdata", "INIT_VMPA_Jan2015", package = "atlantisom")
#' load(file.path(directory, "outputSETASrun_truth.RData"))
#'
#' lenout <- calc_age2length(
#'   structn=result$structn, resn=result$resn, nums=result$nums,
#'   biolprm = result$biolprm, fgs = result$fgs,
#'   CVlenage = 0.1, remove.zeroes=TRUE)
#' }
calc_age2length <- function(structn, resn, nums,
  biolprm, fgs, maxbin = 150,
  CVlenage = 0.1, remove.zeroes = TRUE) {

### Inputs required
### Ages (hard-wired right now for 10 cohorts per group). This can be modified.
ages = 1:10

### Length structure - making very fine (and appropriate for fish),
# but presumably this could be something
# that is pre-specified in the call to the function, and would likely change by species
# Perhaps solution is to make number of length bins fixed across groups, but
# change the upper limits for each length bin
upper.bins <- 1:maxbin

# CV length at age for each species is Needed to create the age-length key.
# This could conceivably be passed to the function with vals for each species.

## Get group codes to calculate size comps for
#groups <- as.factor(fgs$Name)
groups <- unique(as.factor(structn$species)) # use nums instead of structn?

times <- unique(structn$time) # use nums instead of structn?

#calculate mean length at age
mulen <- nums
muweight <- nums
#
# extract rows from structn and resn that match the rows in nums, which are only non-zeroes
resn.ind <- with(resn,paste(species,'.',agecl,'.',polygon,'.',layer,'.',time,sep=""))
num.ind <- with(nums,paste(species,'.',agecl,'.',polygon,'.',layer,'.',time,sep=""))
pick <- match(num.ind,resn.ind)
SRN <- resn$atoutput[pick] + structn$atoutput[pick]

# get weight-length parameters
li_a_use <- biolprm$wl[match(fgs$Code[match(nums$species,fgs$Name)],biolprm$wl[, 1]), 2]
li_b_use <- biolprm$wl[match(fgs$Code[match(nums$species,fgs$Name)],biolprm$wl[, 1]), 3]

#calc mean length and weight at age
mulen$atoutput <- ((biolprm$kgw2d*biolprm$redfieldcn*SRN)/(1000*li_a_use))^(1/li_b_use)
muweight$atoutput <- li_a_use*mulen$atoutput^li_b_use

#
# #calculate length comps
#(numbers at length at max resolution - can then be collapsed to appropriate
#spatial/temporal resolution later)
upper.bins <- 1:maxbin
lower.bins <- c(0,upper.bins[-length(upper.bins)])
lenfreq <- NULL

# small effective sample sizes may return 0 nums for oldest age classes, resulting in NA
# I'd rather keep that in here then change the match function in line 67 to return 0
if(maxbin<max(mulen$atoutput, na.rm = TRUE)){
  print("Warning: maximum bin size is smaller than the longest fish in the sample. Fish above the maximum bin size will be removed from length compositions.")
}
for (irow in 1:nrow(mulen))
#for (irow in 1:500)
  {
  group <- nums$species[irow]
  igroup <- which(groups==group)
  box <- nums$polygon[irow]
  layer <- nums$layer[irow]
  time <- nums$time[irow]
  age <- nums$agecl[irow]

  sigma = sqrt(log((CVlenage^2)+1))
  muuse <- log(mulen$atoutput[irow]) - 0.5*(sigma^2)
  CumFracperbin <- plnorm(upper.bins,muuse,sigma)
  Fracperbin <- c(CumFracperbin[1],diff(CumFracperbin))
  natlength = Fracperbin*nums$atoutput[irow]
  results <- cbind(mulen[irow,],lower.bins,upper.bins, row.names = NULL) # add row.names = NULL?
  results$atoutput <- round(natlength,0)
  #results <- results[results$atoutput>0,]
  lenfreq <- rbind(lenfreq,results) # add row.names = NULL?
}

#get rid of zero rows.
if (remove.zeroes) lenfreq <- lenfreq[lenfreq$atoutput>0,]

#output. natlength now has additional columns because of need to store length bin info
lenout <- NULL
lenout$mulen <- mulen
lenout$muweight <- muweight
lenout$natlength <- lenfreq

return(lenout)
# END THE SIZE COMP FUNCTION HERE.
}

