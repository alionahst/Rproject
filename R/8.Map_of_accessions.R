###--Map of accessions and correlation between ranking and elevation--###

#install.packages("biogeo")
library("biogeo") #important! don't forget to add

#Creating data frame for the future map from Acc_loc table
map.acc <- data.frame(code = Acc_loc$Code,
                      lat = Acc_loc$Latitude,
                      long = Acc_loc$Longitude)
#separating longitude and latitude data nto separate columns according to degrees
#minutes, seconds and cardinals
map.acc <- separate(map.acc, lat, c("lt.d", "lt.m", "lt.s", "lt.cr"))
map.acc <- separate(map.acc, long, c("ln.d", "ln.m", "ln.s", "ln.cr"))
#creating data frame with decimal coordinates
map.acc <- data.frame(code = map.acc$code,
                      Latitude = (dms2dd(as.numeric(map.acc$lt.d),
                                    as.numeric(map.acc$lt.m),
                                    as.numeric(map.acc$lt.s),
                                    map.acc$lt.cr)),
                      Longitude = (dms2dd(as.numeric(map.acc$ln.d),
                                     as.numeric(map.acc$ln.m),
                                     as.numeric(map.acc$ln.s),
                                     map.acc$ln.cr)))
#Taking out "Es-" part from future labels
map.acc$code <- sub('Es-', '', map.acc$code)
#creating data.frame for colors to correspond to ranking of accessions ]
colors <- data.frame(accession = as.numeric(row.names(MFV_Ranked)),
                     mean_MFV = MFV_Ranked$Mean,
                     Category = MFV_Ranked$Category)
colors <- colors[order(colors[,1]),] #ordering the categories according to
#accessions numbers
ggplot(map.acc, aes(x = Longitude, 
                    y = Latitude, 
                    color = as.factor(colors$Category))) +
  labs(colour = "Categories according to salt tolerance ranking") +
  geom_point() +
  geom_text(aes(label = code), nudge_x = -0.2, size = 3, check_overlap = T) +
  borders(database = "world", regions = "Pakistan") +
  ggtitle("Accessions location in Pakistan") +
  theme_minimal()
  
#Checking the correlation between mean MFV (of 6 important traits) and elevation
Elev_Cor <- as.data.frame(MFV$Mean)
Elev_Cor$Elevation <- Acc_loc$Elevation
#printing the correlation results  
cor_1 <- rcorr(as.matrix(Elev_Cor))
cor_1$r[2,1]