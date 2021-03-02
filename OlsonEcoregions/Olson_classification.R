require(rgeos)
require(maptools)
require(raster)
require(rgdal)
require(sp)

require(dplyr)

setwd("~/")
olson <- readOGR("Dropbox/CarlaZach/OlsonEcoregions","wwf_terr_ecos")
tapply(olson$ECO_NAME,olson$BIOME,unique)

# 1 = wet forests
# 2 = dry forests
# 3 = tropical pine forests?
# 4 = temperate forest
# 5 = conifer forests
# 6 = boreal forests
# 7 = savanna
# 8 = grassland
# 9 = flooded savanna
# 10 = alpine grassland
# 11 = tundra
# 12 = MTE
# 13 = desert
# 14 = mangrove
# 98 = lake
# 99 = rock/ice

olson$GRASSY <- "no"
olson$GRASSY[olson$BIOME %in% c("2","3","7","8","9","10")] <- "yes"

olson$GRASSY[olson$ECO_NAME %in% c("Deccan thorn scrub forests",
                                   "Guajira-Barranquilla xeric scrub",
                                   "Sin Valley dry forests",
                                   "Maracaibo dry forests",
                                   "Paraguana xerix scrub",
                                   "Araya and Paria xeric scrub",
                                   "La Costa xeric shrublands",
                                   "Northwestern thorn scrub forests",
                                   "Tamaulipan matorral",
                                   "Tamaulipan mezquital",
                                   "Kalahari xeric savanna",
                                   "Namibian savanna woodlands",
                                   "Madagascar subhumid forests",
                                   "Southeastern conifer forests",
                                   "Florida sand pine scrub")] <- "yes"

biomes <- unionSpatialPolygons(olson,olson$GRASSY)

data <- read.csv("Dropbox/CarlaZach/woody_data.csv")

sites <- data[,3:2]
names(sites) <- c("x", "y")
sts <- coordinates(sites)
sts <- SpatialPointsDataFrame(sts,data.frame(sites = data[,1]))
crs(sts) <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

data$Olson_Ecoregion <- over(sts, olson)
data$Olson_Ecoregion <- data$Olson_Ecoregion[,4]
data$grassy <- over(sts, biomes)

for (i in 1:nrow(data)) {
  if(data$grassy[i] == 1) {
    data$grassy[i] <- "No"
  } else if(data$grassy[i] == 2) {
    data$grassy[i] <- "Yes"
  }
}


sites_classified <- data.frame()
for (i in 1:(nrow(adam)-1)) {
  if(adam$Site[i] != adam$Site[i+1]) {
    sites_classified <- rbind(sites_classified, c(adam$SiteName[i], adam$Latitude[i], 
                                                  adam$Longitude[i], adam$Vegetation[i], 
                                                  adam$SubVeg[i], adam$Olson_Ecoregion[i], 
                                                  adam$grassy[i]))
  }
}

sites_classified <- sites_classified %>% rename(Site = X.Alapaha.Experimental.Range..GA..USA., Latitude = X.31.16666667., Longitude = X..83.25., Adam_Vegetation = X.Needleleaf., Adam_SubVeg = X.Pine., Olson_Ecoregion = X.Southeastern.conifer.forests., Grassy = X.Yes.)











adam <- read.csv("Dropbox/CarlaZach/dataAdam/firevegsumba32-11-2020.csv")

sites <- adam[,22:21]
names(sites) <- c("x", "y")
sts <- coordinates(sites)
sts <- SpatialPointsDataFrame(sts,data.frame(sites = adam[,2]))
crs(sts) <- crs("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

adam$Olson_Ecoregion <- over(sts, olson)
adam$Olson_Ecoregion <- adam$Olson_Ecoregion[,4]
adam$grassy <- over(sts, biomes)

for (i in 1:nrow(adam)) {
  if(adam$grassy[i] == 1) {
    adam$grassy[i] <- "No"
  } else if(adam$grassy[i] == 2) {
    adam$grassy[i] <- "Yes"
  }
}

write.csv(adam, "Dropbox/CarlaZach/dataAdam/sumba_reclassified.csv")

sites_classified <- data.frame()
for (i in 1:(nrow(adam)-1)) {
  if(adam$Site[i] != adam$Site[i+1]) {
    sites_classified <- rbind(sites_classified, c(adam$SiteName[i], adam$Latitude[i], 
                                                  adam$Longitude[i], adam$Vegetation[i], 
                                                  adam$SubVeg[i], adam$Olson_Ecoregion[i], 
                                                  adam$grassy[i]))
  }
}

sites_classified <- sites_classified %>% rename(Site = X.Alapaha.Experimental.Range..GA..USA., Latitude = X.31.16666667., Longitude = X..83.25., Adam_Vegetation = X.Needleleaf., Adam_SubVeg = X.Pine., Olson_Ecoregion = X.Southeastern.conifer.forests., Grassy = X.Yes.)

write.csv(sites_classified, "Dropbox/CarlaZach/dataAdam/siteclassification.csv")
