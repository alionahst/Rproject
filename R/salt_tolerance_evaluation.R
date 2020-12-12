#salt tolerance index for morphological traits

x <- data.frame(dplyr::filter(table, table$Treatment=="Treatment"))
x <- x[4:8]
y <- data.frame(dplyr::filter(table, table$Treatment=="Control"))
y <- y[4:8]

STI_Shoot_Length <- x[1]/y[1]
STI_Root_Length <- x[2]/y[2]
STI_Plant_Height <- x[3]/y[3]
STI_Number_Leaves <- x[4]/y[4]
STI_Leaf_Area <- x[5]/y[5]
STI <- data.frame(STI_Shoot_Length,STI_Root_Length, STI_Plant_Height, STI_Number_Leaves, STI_Leaf_Area)

Number<- c(1:6, 8:15,17:25)
STI$Number <- Number




#Membership function value

x <- numeric()
for (i in 1:23){
  a <- (STI$STI_Shoot_Length[i] - min(STI$STI_Shoot_Length))/(max(STI$STI_Shoot_Length)- min(STI$STI_Shoot_Length))
  x <- c(x, a)
}

x <- data.frame(x, Number)


