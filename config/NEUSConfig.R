d.name <- here("atlantisoutput","NEUStest20160303")
functional.groups.file <- "NeusGroups.csv" 
biomass.pools.file <- ""
biol.prm.file <- "at_biol_neus_v15_DE.prm"
box.file <- "neus30_2006.bgm"
initial.conditions.file <- "inneus_2012.nc"
run.prm.file <- "at_run_neus_v15_DE.xml"
scenario.name <- "neusDynEffort_Test1_"
truth.file <- "outputneusDynEffort_Test1_run_truth.RData" 


timeall <- c(0:251)
funct.group.indices <- c(1:21)
atlantis.output <- here("atlantisoutput","NEUStest20160303","neusDynEffort_Test1_BiomIndx.txt")