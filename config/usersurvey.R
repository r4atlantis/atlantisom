# Default survey configuration here is a census
# Need to define survey season, area, efficiency, selectivity

# a survey that takes place once per year mid year has now been declared default
annualmidyear <- seq(midptyr, noutsteps, stepperyr)
survtime <- annualmidyear

# survey area
# should return all model areas
survboxes <- allboxes

# survey efficiency (q)
# should return a perfectly efficient survey 
surveffic <- data.frame(species=survspp,
                     efficiency=rep(1.0,length(survspp)))

# survey selectivity (agecl based)
# this is by age class, need to change to use with ANNAGEBIO output
survselex <- data.frame(species=rep(survspp, each=n_age_classes),
                     agecl=rep(c(1:n_age_classes),length(survspp)),
                     selex=rep(1.0,length(survspp)*n_age_classes))

# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
surveffN <- data.frame(species=survspp, effN=rep(1e+8, length(survspp)))

# survey index cv needed for sample_survey_xxx
# perfect observation
surv_cv <- data.frame(species=survspp, cv=rep(0.0,length(survspp)))

# length at age cv for input into calc_age2length function
# function designed to take one cv for all species, need to change to pass it a vector
lenage_cv <- 0.1

# max size bin for length estimation, function defaults to 150 cm if not supplied
maxbin <- 150

