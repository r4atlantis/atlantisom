#'
#' @author Gavin Fay
#' calculate length composition from Atlantis output (st)age data
#' Uses numbers at age, plus weight at age, and weight-length relationships to
#' generate size comps
#'
#' @param resn dataframe of reserve n by species, age class, box, depth,
#' and time
#' @template structn
#' @template resn
#' @template nums
#' @template biolprm
#' @template fgs
#' The above should  be changed to only pass the pieces from these that
#' we need.
#' @param CVlenage The variability in length at age (currently same for all species)
#' @param remove.zeroes  Logical asking whether to only return numbers at length for
#' combinations of species, age, box, depth, etc that have numbers>0
#' @return A \code{list} containing three \code{data.frame}s, mulen (mean length at age),
#' muweight (mean weight at age), and natlength (numbers at length). natlength is in the
#' same format as other dataframes in the atlantisom package except has two additional
#' columns to include the length bin information.
calc_age2length <- function(structn, resn, nums,
  biolprm, fgs,
  CVlenage = 0.1, remove.zeroes = TRUE) {

### Inputs required
### Ages (hard-wired right now for 10 cohorts per group). This can be modified.
ages = 1:10

### Length structure - making very fine (and appropriate for fish),
# but presumably this could be something
# that is pre-specified in the call to the function, and would likely change by species
# Perhaps solution is to make number of length bins fixed across groups, but
# change the upper limits for each length bin
upper.bins <- 1:150

# CV length at age for each species is Needed to create the age-length key.
# This could conceivably be passed to the function with vals for each species.

## Get group codes to calculate size comps for
groups <- as.factor(fgs$Name)

times <- unique(structn$time)

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
upper.bins <- 1:150
lower.bins <- c(0,upper.bins[-length(upper.bins)])
lenfreq <- NULL
#for (irow in 1:nrow(mulen))
for (irow in 1:100)
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
  results <- cbind(mulen[irow,],lower.bins,upper.bins)
  results$atoutput <- natlength
  #results <- results[results$atoutput>0,]
  lenfreq <- rbind(lenfreq,results)
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


# #example on test data set
# #
# dir <- "~/Atlantis/r4atlantis/atlantisom/inst/extdata/INIT_VMPA_Jan2015"
# biolprm <- load_biolprm(dir=NULL,file_biolprm=file.path(dir,"VMPA_setas_biol_fishing_Trunk.prm",fsep="/"))
# groups <- load_groups(file.path(dir,'functionalGroups.csv',fsep="/"))
# bps <- load_box(dir=NULL,file_bgm=file.path(dir,"VMPA_setas.bgm",fsep="/")) #,
# groupfile <- file.path(dir,'functionalGroups.csv',fsep="/")
# ncfile <- file.path(dir,'outputSETAS.nc',fsep="/")
#
# reserven <- load_nc(dir=NULL,
#              file_nc=ncfile,
#              fgs=read.table(groupfile,sep=",",header=TRUE),
#              bps=bps,select_groups=groups,
#               select_variable="ResN",
#               check_acronyms=TRUE)
# structn <- load_nc(dir=NULL,
#                     file_nc=ncfile,
#                     fgs=read.csv(groupfile),
#                     bps=bps,select_groups=groups,
#                     select_variable="StructN",
#                     check_acronyms=TRUE)
# nums <- load_nc(dir=NULL,
#                    file_nc=ncfile,
#                    fgs=read.csv(groupfile),
#                    bps=bps,select_groups=groups,
#                    select_variable="Nums",
#                    check_acronyms=TRUE)
#
# lenout <- calc_age2length(structn=structn,
#                           reserven=reserven,
#                           nums=nums,
#                             biolprm=biolprm,
#                             ncfile=ncfile,
#                             fgs=read.csv(groupfile),
#                             CVlenage=0.1,
#                             remove.zeroes=TRUE)
#
