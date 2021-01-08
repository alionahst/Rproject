##--experimenting with mapping the data--##
library("tmap")
#install.packages("tmap")
library("tmaptools")
library("sf")
library("leaflet")
library("ggplot2")
library("biogeo")
library("tidyr")
library("raster")
library("dplyr")
library("spData")
library("spDataLarge")
#install.packages('spDataLarge', repos='https://nowosad.github.io/drat/', type='source')
map.acc <- data.frame(code = Acc_loc$Code,
                      lat = Acc_loc$Latitude,
                      long = Acc_loc$Longitude)

map.acc <- separate(map.acc, lat, c("lt.d", "lt.m", "lt.s", "lt.cr"))
map.acc <- separate(map.acc, long, c("ln.d", "ln.m", "ln.s", "ln.cr"))

map.acc <- data.frame(code = map.acc$code,
                      lat = (dms2dd(as.numeric(map.acc$lt.d),
                                    as.numeric(map.acc$lt.m),
                                    as.numeric(map.acc$lt.s),
                                    map.acc$lt.cr)),
                      long = (dms2dd(as.numeric(map.acc$ln.d),
                                     as.numeric(map.acc$ln.m),
                                     as.numeric(map.acc$ln.s),
                                     map.acc$ln.cr)))

library(maps)
install.packages("rnaturalearth")
library("rnaturalearth")
#devtools::install_github("ropensci/rnaturalearthhires")
#install.packages("rnaturalearthhires", repos = "http://packages.ropensci.org")
library("rnaturalearthhires")
#install.packages("mapdata")
library(mapdata)

library(sf)
library(tmap)
library(readxl)
library(raster)
library(mapview)
library(classInt)
library(gapminder)
library(tidyverse)
library(googlesheets4)
library(rnaturalearth)



Pakistan <- ne_countries(scale = 'large', country = 'pakistan') %>% st_as_sf()
plot(Pakistan)

map1 <- ggplot(Pakistan)
map1 + geom_path(linestyle = 2)
map1 + geom_text(map.acc, aes(label = code, x = lat, y = long))
#tm_text('name_ru', size = 0.5, remove.overlap = TRUE, auto.placement = TRUE)

Pakistan <- data.frame(map("world", "Pakistan", plot=FALSE)[c("x","y")])
map1 <- ggplot(Pakistan, aes(x=x, y=y)) +
  geom_path(linestyle = 2) +
  geom_text(map.acc, aes(label = code, x = lat, y = long)) +
  coord_map() + 
  theme_bw()
map1

  scale_fill_gradient(high = "green", low = "red", guide = "colorbar") +
  coord_equal() +
  theme() +
  ggtitle("Title")
p + labs(map.acc$code)
p + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
p + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
p + theme(panel.border = element_blank())
print(p)

points(map.acc$long, map.acc$lat, pch=19, col=map.acc$code, cex=0.5)  #plot my sample sites
#map.scale(69, 30,relwidth = 0.001, metric = TRUE, ratio = TRUE)



library(maps)
library(mapdata)
library(maptools)  #for shapefiles
library(scales)  #for transparency
pcontorta <- readShapePoly("pinucont.shp")   #layer of data for species range
samps <- read.csv("FieldSamples.csv")   #my data for sampling sites, contains a column of "lat" and a column of "lon" with GPS points in decimal degrees
map("worldHires","Canada", xlim=c(-140,-110),ylim=c(48,64), col="gray90", fill=TRUE)  #plot the region of Canada I want
map("worldHires","usa", xlim=c(-140,-110),ylim=c(48,64), col="gray95", fill=TRUE, add=TRUE)  #add the adjacent parts of the US; can't forget my homeland
plot(pcontorta, add=TRUE, xlim=c(-140,-110),ylim=c(48,64), col=alpha("darkgreen", 0.6), border=FALSE)  #plot the species range
points(samps$lon, samps$lat, pch=19, col="red", cex=0.5)  #plot my sample sites



ggplot(map.acc, aes(x = long, y = lat)) +
  geom_polygon(data=map.acc, aes(x=long, y=lat), group = 1)+
  coord_quickmap()+
  geom_text(aes(label = code), data = map.acc,  size = 3, hjust = 0.5)+
  scale_fill_viridis_d()+
  theme_void()+
  theme(legend.position = "none")

m <- ggplot(map.acc, aes(x = lat,
                         y = long))
m +
  geom_jitter() +
  geom_label(aes(label = code))

