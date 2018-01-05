library(tidyverse)
library(RColorBrewer)


# Create data.frames of unqiue voters in each type of election
genUnique <- general[!duplicated(general[,"fullName"]),]
muniUnique <- municipal[!duplicated(municipal[,"fullName"]),]


ggplot(genUnique, aes(x = age, color = statusCode)) +
    geom_histogram(bins = 20, fill = 'black') +
    labs(x = 'Age', y = 'Count')


ggplot(muniUnique, aes(age, fill = statusCode)) +
    geom_histogram(bins = 20, color = 'black')+
    scale_fill_manual(values = c("blue","red","green")) +
    labs(x = 'Age', y = 'Voter Count', title = "Municipal Election Voters by Age")
  
    

    #geom_histogram(aes(fill=statusCode),
                  # bins = 20,
                   #fill = 'black') 
  

# Age in each Town during each election
genUnique %>% filter(municipalityName == "CARRBORO") %>% select(age) %>% lapply(., mean, na.rm = T)
genUnique %>% filter(municipalityName == "CHAPEL HILL") %>% select(age) %>% lapply(., mean, na.rm = T)
# Carrboro: 43, Chapel Hill: 38.5

muniUnique %>% filter(municipalityName == "CARRBORO") %>% select(age) %>% lapply(., mean, na.rm = T)
muniUnique %>% filter(municipalityName == "CHAPEL HILL") %>% select(age) %>% lapply(., mean, na.rm = T)
# Carrboro: 52, Chapel HillL: 57

# Number of registered voters who voted over the age of age 65 compared to 18-34 
muniUnique %>% select(age)%>% filter(. >= 65) %>% nrow(.)
muniUnique %>% select(age)%>% filter(.>= 18 & .<= 34)%>% nrow(.)
# Nearly 2.5x as many Over 65 voters voted in the municipal election than the general 


genUnique %>% select(age)%>% filter(. >= 65) %>% nrow(.)
genUnique %>% select(age)%>% filter(.>= 18 & .<= 34)%>% nrow(.)
# 4.0x as many under 34 voters voted than over 65 in the general 


# Average age of active voters compared to average of all registered voters
data.combined %>% filter(statusCode == "A") %>% select(age) %>% lapply(., mean, na.rm = T)
data.combined %>% filter(statusCode == "I") %>% select(age) %>% lapply(., mean, na.rm = T)

# Percent Active / Total 
filter(data.combined, statusCode == "A") %>% nrow()
filter(data.combined, statusCode == "I") %>% nrow()
filter(data.combined, statusCode == "S") %>% nrow()

# address to polling place input/ flask
# prerendered maps
#greensboro/ open data

# interesting facts
# overlays
# race and socioeconomic data by precinct 
# chatham






















