d.name <- here("atlantisoutput","CalCurrentSummitScenario1")
functional.groups.file <- "CalCurrentV3Groups.csv"
biomass.pools.file <- "DIVCalCurrentV3_BIOL.nc"
biol.prm.file <- "CalCurrentV3_Biol.prm"
box.file <- "CalCurrentV3_utm.bgm"
initial.conditions.file <- "DIVCalCurrentV3_BIOL.nc"
run.prm.file <- "CalCurrentV3_run.xml"
scenario.name <- "CCV3"
truth.file <- "outputCCV3run_truth.RData"


timeall <- c(0:100)
funct.group.indices <- c(1:44, 59:61, 65:68)
atlantis.output <- here("atlantisoutput","CalCurrentSummitScenario1","outputCCV3BiomIndx.txt")