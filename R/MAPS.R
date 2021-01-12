##--experimenting with mapping the data--##
library("tmap")
#install.packages("tmap")
library("tmaptools")
library("sf")
library("leaflet")
library("ggplot2")
install.packages("biogeo")
library("biogeo") #important! don't forget to add
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
                      latitude = (dms2dd(as.numeric(map.acc$lt.d),
                                    as.numeric(map.acc$lt.m),
                                    as.numeric(map.acc$lt.s),
                                    map.acc$lt.cr)),
                      longitude = (dms2dd(as.numeric(map.acc$ln.d),
                                     as.numeric(map.acc$ln.m),
                                     as.numeric(map.acc$ln.s),
                                     map.acc$ln.cr)),
                      ranking = MFV_Ranked$Category)
map.acc$code <- sub('Es-', '', map.acc$code)

ggplot(map.acc, aes(x = long, y = lat)) +
  geom_point() +
  geom_text(aes(label = code), nudge_x = -0.2, size = 3, check_overlap = T) +
  borders(database = "world", regions = "Pakistan") +
  theme_minimal()