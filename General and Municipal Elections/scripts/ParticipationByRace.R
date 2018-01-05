library(tidyverse)

participationRate <- function(df, municipality) {
    
races <- c("A", "B", "M", "I", "O","W")

summary <- df %>% 
    filter(municipalityName == municipality, race %in% races) %>%
    group_by(race) %>%
    summarise(
        count = n()
    )

popByRace <- c(5785, 4630, 185, 905, 2650, 35075)

summary[1] <- c("Asian", "Black", "Native American", "Multi-Racial", "Other/Unknown", "White")
summary[3] <- (summary[2] / popByRace)
summary <- rename(summary, participationRate = `count.1`)

list <- list(race = summary[1], participationRate =  summary[3])
summary

}

participationRate(municipal, "CHAPEL HILL")
participationRate(general, "CHAPEL HILL")



# NOTES ON DATA -----------------------------------------------------------


## Population numbers from the 2011-2015 American Community Survey 5 year estimates - published 02/01/2017
# https://www.census.gov/rdo/data/voting_age_population_by_citizenship_and_race_cvap.html

## Voting age population in the Town of Chapel Hill by race

# Total		49230
# American Indian or Alaska Native Alone	170
# Asian Alone	5785
# Black or African American 4630
# Native Hawaiian or Other Pacific Islander Alone		15
# White Alone		35075
# American Indian or Alaska Native and White	85
# Asian and White	490
# Black or African American and White		165
# American Indian or Alaska Native and Black or African American	25
# Remainder of Two or More Race Responses		140
