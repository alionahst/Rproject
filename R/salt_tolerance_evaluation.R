#salt tolerance index 


# THIS WORKS !! but we need to do 5 times
a1<- subset(table$Shoot_Length, table$Treatment=="Control")
a2<- subset(table$Shoot_Length, table$Treatment=="Treatment")

a3<- subset(table$Root_Length, table$Treatment=="Control")
a4<- subset(table$Root_Length, table$Treatment=="Treatment")

a5<- subset(table$Plant_Height, table$Treatment=="Control")
a6<- subset(table$Plant_Height, table$Treatment=="Treatment")

a7<- subset(table$Number_Leaves, table$Treatment=="Control")
a8<- subset(table$Number_Leaves, table$Treatment=="Treatment")

a9<- subset(table$Leaf_Area, table$Treatment=="Control")
a10<- subset(table$Leaf_Area, table$Treatment=="Treatment")

STI <- data.frame(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10, table$Code)
STI <- tab1 %>%
  mutate(STI_Shoot_Length = a2/a1,
         STI_Root_Length = a4/a3,
         STI_Plant_Height = a6/a5,
         STI_Number_Leaves = a8/a7,
         STI_Leaf_Area = a10/a9) %>%
  select (STI_Shoot_Length, STI_Root_Length, STI_Plant_Height, STI_Number_Leaves, STI_Leaf_Area)

Number<- c(1:6, 8:15,17:25)
STI$Number <- Number



# try to make loop
#for (i in names(table[4:8])){
#  for (j in new){
#    a1<- subset(table$[i], table$Treatment=="Control")
#    a2<- subset(table$[i], table$Treatment=="Treatment")
#  }
#}



#Membership function value

x <- numeric()
for (i in 1:23){
  a <- (STI$STI_Shoot_Length[i] - min(STI$STI_Shoot_Length))/(max(STI$STI_Shoot_Length)- min(STI$STI_Shoot_Length))
  x <- c(x, a)
}

x <- data.frame(x, Number)


