require(rgeos)
require(maptools)
require(raster)
require(rgdal)
require(sp)
require(dplyr)

setwd("~/Desktop/Meta-Analysis")
olson <- readOGR("OlsonEcoregions","wwf_terr_ecos")
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

data <- read.csv("Woody Data.csv")

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
for (i in 1:(nrow(data)-1)) {
  if(data$Site[i] != data$Site[i+1]) {
    sites_classified <- rbind(sites_classified, c(data$Site[i], data$Latitude[i], 
                                                  data$Longitude[i], data$Vegetation[i], 
                                                  data$Olson_Ecoregion[i], 
                                                  data$grassy[i]))
  }
}

sites_classified <- sites_classified %>% rename(Site = X.Alapaha., Latitude = X.31.16666667., Longitude = X..83.25., Olson_Ecoregion = X.Southeastern.conifer.forests., Grassy = X.Yes.)

# library(rworldmap)
# library(ggplot2)
# world <- getMap(resolution = "low")

# ggplot() + geom_polygon(data = world, aes(x = long, y = lat, group = group),
#                         fill = NA, colour = "gray") + 
#   geom_point(data = data, aes(x = Longitude, y = Latitude, color = grassy), 
#              pch = 4) + 
#   coord_quickmap() + ylim(-60, 90) + theme_classic() + xlab("Longitude") +
#   ylab("Latitude") + geom_jitter()

require(raster)
r <- getData("worldclim", var="bio", res=10)
r <- r[[c(1,12)]]
names(r) <- c("Temp", "Precip")

values <- extract(r,sts)
df <- cbind.data.frame(coordinates(sts),values)
plot(r[[2]])
plot(sts,add=T)

write.csv(data, "Woody.csv")

# Same thing for grassy data

data <- read.csv("Herbaceous Data.csv")

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
for (i in 1:(nrow(data)-1)) {
  if(data$Site[i] != data$Site[i+1]) {
    sites_classified <- rbind(sites_classified, c(data$Site[i], data$Latitude[i], 
                                                  data$Longitude[i], data$Vegetation[i], 
                                                  data$Olson_Ecoregion[i], 
                                                  data$grassy[i]))
  }
}

sites_classified <- sites_classified %>% rename(Site = X.Alapaha., Latitude = X.31.16666667., Longitude = X..83.25., Olson_Ecoregion = X.Southeastern.conifer.forests., Grassy = X.Yes.)

require(raster)
r <- getData("worldclim", var="bio", res=10)
r <- r[[c(1,12)]]
names(r) <- c("Temp", "Precip")

values <- extract(r,sts)
df <- cbind.data.frame(coordinates(sts),values)
plot(r[[2]])
plot(sts,add=T)

write.csv(data, "Herbaceous.csv")
