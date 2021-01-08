Pakistan <- data.frame(map("world", "Pakistan", plot=FALSE)[c("x","y")])

p <- ggplot(Pakistan, aes(x=x, y=y)) +
  geom_path(linestyle = 2) +
  coord_map() + theme_bw()
p + labs(map.acc$code)
p + theme(panel.grid.minor=element_blank(), panel.grid.major=element_blank())
p + theme(axis.ticks = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank())
p + theme(panel.border = element_blank())
print(p)

library(mapproj)
#install.packages("mapproj")
library("maps")
#install.packages("ggmap")
library("ggmap")
library("ggplot2")

Country <- "Pakistan"





Country2 <- Country1 + geom_polygon(data = Pakistan
                                    , aes(x=x, y=y)
                                    , color = 'white', alpha = .75, size = .2)

print(Country2)

