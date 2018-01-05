library(rgdal)
library(ggmap)
library(RDSTK)
library(tidyverse)
library(stringr)

## TODO: 
# 1. GoogleMaps API, Driving Directions
# 3. https://rpubs.com/nickbearman/r-google-map-making
# 4. https://gist.github.com/jjkrol/5894869
# 5. https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles
# 6. https://www.r-bloggers.com/amateur-mapmaking-getting-started-with-shapefiles/
# 7.  look up how to do overlays

## https://www.youtube.com/watch?v=EtJ-iTZeqTg
#https://stackoverflow.com/questions/13762793/plotting-choropleth-maps-from-kml-data-using-ggplot2
#https://www.nceas.ucsb.edu/scicomp/usecases/shapeFileToKML
#https://github.com/tidyverse/ggplot2/wiki/plotting-polygon-shapefiles

# Read in .KML used to create precinct boundary
googlePrecincts = 'data/Precincts.kml'
lyr <- ogrListLayers(googlePrecincts)
plt <- readOGR(googlePrecincts, layer = lyr) %>% fortify(.)

# Create full length addresses

    fullAddress <- data.combined[!duplicated(data.combined[,"fullName"]),7:8]
    fullAddress$residentialStreet <- gsub('# ', '',fullAddress$residentialStreet)
    fullAddress <- transmute(fullAddress, address = paste(residentialStreet, residentialCity))
    
    coords <- data.frame(latitude = double(), longitude = double())
    #fullAddress <- filter(fullAddress,str_detect(address, "CARRBORO"))


for (i in 1:nrow(fullAddress)){
   tryCatch({
    address <- (toString(fullAddress[i,1])) 
    Encoding(address)<- 'latin1'
    latLong <- street2coordinates(address)
    coords[i,] <- select(latLong, latitude, longitude)
   },
   
   error = function(e){cat("ERROR :", conditionMessage(e),"\n")})
  
}
# Plotting Carrboro Coordinates
write.csv(file = "temp/carrboroCoordinates", x = coords)

read_csv("temp/carrboroCoordinates") %>% write.csv(file = "data/carrboroCoordinates", x = .)
carrboroCoords <- read_csv("data/carrboroCoordinates") %>% select(-c(X1,X1_1))
qmap('Carrboro Farmer\'s Market, Carrboro, NC', zoom = 13, maptype = 'satellite')+
    geom_polygon(data = plt,
                 aes(x = long, y = lat, group = group),
                 color = 'gray', fill = NA) +
    geom_point(data = carrboroCoords, aes(x = longitude, y = latitude), color = 'red',size = 1.5, alpha = .5)


# Plotting ch voters
#write.csv(file = "temp/chCoordinates", x = coords)

chCoords <- read_csv("temp/chCoordinates") %>% select(-c(X1,X1_1)) %>% write.csv(file = "data/chCoordinates", x = .)
chCoords <- read_csv("data/chCoordinates")
chCoords <- rename(chCoords, Latitude = `longitude`, Longitude = `latitude`)

qmap('Flyleaf Books, Chapel Hill, NC', zoom = 13, maptype = 'satellite')+
    geom_polygon(data = plt,
                 aes(x = long, y = lat, group = group),
                 color = 'gray', fill = NA) +
    geom_point(data = chCoords, aes(x = latitude, y = longitude), color = 'red',size = 1.5, alpha = .3)


## Try out ggmap 
#mymap <- get_map('Chapel Hill', zoom = 13)


map2 <- ggplot() + 
    geom_polygon(data = plt,
                 aes(x = long, y = lat, group = group),
                 color = 'gray', fill = 'white') +
    geom_point(data = chPlots, aes(x = longitude, y = latitude), color = 'red',size = 2, alpha = .5)
plot(map2)



# Plotting walking routes 
from <- '201 S Columbia St, Chapel Hill, NC 27599 '
to <- 'Cat\'s Cradle'
route <- route(from, to, structure = 'route', mode = 'walking')

qmap('West Franklin Street, Chapel Hill, NC', zoom = 15, maptype = 'satellite') +
    geom_path(
        aes(x = lon, y = lat),  colour = 'red', size = 1.5,
        data = route, lineend = 'round')


map1 <- ggplot() +
    geom_polygon(data = plt,
                 aes(x = long, y = lat, group = group),
                 color = 'gray',fill = 'white', size = .2)
plot(map1)

# Read in .shp file and convert to data.frame
# precincts <- readOGR("data/PrecinctBoundaries", layer = "Voting_Precincst") %>% fortify(.)

reese 
gradient
street names
