d.name <- getwd()
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

funct.groups <- load_fgs(dir=d.name,
                         file_fgs = functional.groups.file)
#Get just the names of active functional groups
funct.group.names <- funct.groups %>%
  filter(IsTurnedOn == 1) %>%
  select(Name) %>%
  .$Name
