d.name <- here("atlantisoutput","NOBACERESGlobalSustainability")
functional.groups.file <- "nordic_groups_v04.csv" 
biomass.pools.file <- "nordic_biol_v23.nc"
biol.prm.file <- "nordic_biol_incl_harv_v_007_3.prm"
box.file <- "Nordic02.bgm"
initial.conditions.file <- "nordic_biol_v23.nc"
run.prm.file <- "nordic_run_v01.xml"
scenario.name <- "nordic_runresults_01"
d.name <- here("atlantisoutput","NOBACERESGlobalSustainability")
truth.file <- "outputnordic_runresults_01run_truth.RData" 

timeall <- c(0:560)
funct.group.indices <- c(1:36)
atlantis.output <- here("atlantisoutput","NOBACERESGlobalSustainability","outputnordic_runresults_01BiomIndx.txt")