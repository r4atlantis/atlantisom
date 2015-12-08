#' Code to get size at age and plot
#' @author Gavin Fay
#' calculate length composition from Atlantis output (st)age data
#' Uses numbers at age, plus weight at age, and weight-length relationships to 
#' generate size comps
#' @param natage dataframe of numbers at age by species, age class, box, depth, 
#' and time
#' @param structn dataframe of structural n by species, age class, box, depth, 
#' and time
#' @param reserven dataframe of reserve n by species, age class, box, depth, 
#' and time
#' @param ncfile full path and filename of biology .prm file
#' @param biolfile full path and filename of biology .prm file
#' @template fgs 
#' The above should  be changed to only pass the pieces from these that
#' we need.
#' @param CVlenage The variability in length at age (currently same for all species)
calc_age2length <- function(natage,
                            structn,
                            reserven,
                            ncfile,
                            biolfile,
                            fgs, # this calls the group csv file already brought in using run_atlantis
                            #groupfile,
                            CVlenage=0.01) {

### Inputs required
# These are available in the biol.prm file.
Kwet = 20.0
Redfield_CN = 5.7
### Ages (hard-wired right now for 10 cohorts per group). This can be modified.
ages = 1:10

### Length structure - making very fine (and appropriate for fish), 
# but presumably this could be something
# that is pre-specified in the call to the function, and would likely change by species
# Perhaps solution is to make number of length bins fixed across groups, but
# change the upper limits for each length bin
upper.bins <- 1:150

# CV length at age for each species. Needed to create the age-length key.
# This could conceivably be passed to the function for each species.
#CVlenage = 0.01 #15
#CVlenage <- array(0.01,dim=c(length(groups),length(ages)))

## Get group codes to calculate size comps for
groups <- as.factor(fgs$Name)

### Get the weight-length relationship parameters.
# Currently assuming they are fixed inputs and not changing over time.
# read in biology file
TheData <- read.table(biolfile,col.names=1:100,comment.char="",fill=TRUE,header=FALSE)
#find the weight-length parameters from the biology prm file, store them
pick <- grep("li_a_",TheData[,1])
xx <- TheData[pick,1:20]
tempmat <- matrix(NA,nrow=nrow(xx),ncol=3)
for (igroup in 1:nrow(tempmat)) tempmat[igroup,1] <- strsplit(as.character(xx[igroup,1]),"li_a_")[[1]][2]
tempmat[,2] <- as.numeric(as.character(xx[,2]))
pick <- grep("li_b_",TheData[,1])
tempmat[,3] <- as.numeric(as.character(TheData[pick,2]))

#find the ages per cohort parameters from the biology prm file, store them
pick <- grep("_AgeClassSize",TheData[,1])
xx <- TheData[pick,1:20]
ages.per.cohort <- matrix(NA,nrow=nrow(xx),ncol=2)
for (igroup in 1:nrow(ages.per.cohort)) ages.per.cohort[igroup,1] <- 
  strsplit(as.character(xx[igroup,1]),"_AgeClassSize")[[1]][1]
ages.per.cohort[,2] <- as.numeric(as.character(xx[,2]))

#Here is where you could be clever with regard to stage/ages.
# Think this is better handled external to this function:
# that is, the Numbers and Size at age passed to here have handled the conversion from stages to ages.
#group.names$NumAgeClassSize

#find the ages at maturity parameters from the old prm file, store them.
# redundant code, not used for the size comp calcs, legacy from GF previous script.
pick <- grep("_age_mat",TheData[,1])
xx <- TheData[pick,1:20]
age.of.maturity <- matrix(NA,nrow=nrow(xx),ncol=2)
for (igroup in 1:nrow(ages.per.cohort)) age.of.maturity[igroup,1] <- 
  strsplit(as.character(xx[igroup,1]),"_age_mat")[[1]][1]
age.of.maturity[,2] <- as.numeric(as.character(xx[,2]))


# For each Atlantis age class, calculate length associated with weight
# Generate length distribution assuming a fixed CV of length at age

####
# Get the Weights at age and Numbers at age matrices
####
require(ncdf)
xx2 <- open.ncdf(ncfile)
names2 <- rep(NA,xx2$nvar)
for (i in 1:xx2$nvar) names2[i] <- xx2$var[[i]]$name

group <- groups[1] #"Demersal_D_Fish"
#get total N
#groupN <- get.var.ncdf(xx2,paste(group,"_N",sep=""))
#vol <- get.var.ncdf(xx2,"volume")
#TotN <- sum(groupN*vol,na.rm=TRUE)
#This just gets the dimensions, could also get them from 
for (age in 1:10)
{
  SRN <- get.var.ncdf(xx2,paste(group,age,"_ResN",sep=""))
  SRN <- SRN + get.var.ncdf(xx2,paste(group,age,"_StructN",sep=""))
  Nums <- get.var.ncdf(xx2,paste(group,age,"_Nums",sep=""))
  if (age==1) TheN <- SRN*Nums
  if (age>1) TheN <- TheN + SRN*Nums
}

times <- get.var.ncdf(xx2,"t")


#Define size comp storage array for length fequencies
# dimensions: species, Len bins, Depth, Box, Time
Lenfreq = array(0,dim=c(length(groups),
                        length(upper.bins),
                        dim(SRN)),
                dimnames=list(groups=groups,
                length=upper.bins,
                depth=1:dim(SRN)[1],
                box=0:(dim(SRN)[2]-1),
                time=1:dim(SRN)[3]))
# temporary storage array for distribution of numbers for each age.
Fracperbin = array(0,dim=c(length(upper.bins),dim(SRN)))

### fixed variable values, used for debugging/testing
#li_a_use = 0.0107
#li_b_use = 2.91
#Kwet = 20.0
#Redfield_CN = 5.7
#ages = 1:10
#group <- groups[1]

#arrays for mean size at age (weight and length) 
mulenage <- array(0,dim=c(length(groups),10,dim(SRN)[3]))
muweight <- array(0,dim=c(length(groups),10,dim(SRN)[3]))

# Start the calculations
# Loop over groups 
for (group in groups)
{
  print(group)
  igroup <- which(groups==group)
  ncgroup <- group

  #Get the appropriate weight-at-length parameters for this group  
  li_a_use <- 
    as.numeric(tempmat[which(tempmat[,1]==group.names$Code[igroup]),2])
  li_b_use <- 
    as.numeric(tempmat[which(tempmat[,1]==group.names$Code[igroup]),3])
  
  #calculate length
  #weght-length determined by equation #95 in NMFS TechMemo 218, Link et al. 2010

  # loop over ages
  for (age in ages)
  {
    print(age)
    SRN <- get.var.ncdf(xx2,paste(ncgroup,age,"_ResN",sep=""))
    SRN <- SRN + get.var.ncdf(xx2,paste(ncgroup,age,"_StructN",sep=""))
    Nums <- get.var.ncdf(xx2,paste(ncgroup,age,"_Nums",sep=""))
    
    Nperage <- SRN*Nums
        
    #GET LENGTH FOR THIS AGE CLASS
    Length <- ((Kwet*Redfield_CN*SRN)/(1000*li_a_use))^(1/li_b_use)
    # also produce a summary for each time step
    mulen <- rep(0,length(Length[1,1,]))
    for (t in 1:length(mulen))
     {
      numfreq <- Nums[,,t]/sum(Nums[,,t]) 
      mulen[t] <- sum(Length[,,t]*numfreq)      
     }    
    mulenage[igroup,age,] <- mulen
    muweight[igroup,age,] <- li_a_use*mulen^li_b_use

    # Now create the age-length key based on the assumed distribution of length at age
    # Some modification will be required here if CVlenage is not a scalar
    sigma = sqrt(log((CVlenage^2)+1))
    muuse <- log(Length) - 0.5*(sigma^2)
    for (i in 1:dim(SRN)[1])
      for (j in 1:dim(SRN)[2])
        for (k in 1:dim(SRN)[3])
        {
          CumFracperbin <- plnorm(upper.bins,muuse[i,j,k],sigma)
          Fracperbin[,i,j,k] <- c(CumFracperbin[1],diff(CumFracperbin))
        }
    #Now use the ALK and the Numbers at age to get the Length frequencies
    for (i in 1:dim(SRN)[1])
      for (j in 1:dim(SRN)[2])
        for (k in 1:dim(SRN)[3])
        {
          Lenfreq[igroup,,i,j,k] = Lenfreq[igroup,,i,j,k] + Fracperbin[,i,j,k]*Nums[i,j,k]
        }
    
    #close the age loop
  }  
  #close the species loop
}

close.ncdf(xx2)
#save the resulting length frequency array, this is probably what needs to be 
#returned from a call to the function
#save(Lenfreq,file="Lenfreq_Numbers.RData")

#convert to data frame - need to add a column with lower length bin limit
lendat <- as.vector(aperm(Lenfreq,c(2,5,4,3,1)))
thecols <- expand.grid(upper.bins,1:dim(SRN)[3],0:(dim(SRN)[2]-1),1:7,groups)
lenout <- cbind(thecols[,c(5,1,3,4,2)],lendat)
lenout[,5] <- times[lenout[,5]]
names(lenout) <- c("species","upperlength","polygon","layer","time","natlength")
#get rid of zero rows.
lenout <- lenout[lenout$natlength>0,]
return(lenout)
# END THE SIZE COMP FUNCTION HERE.
}



