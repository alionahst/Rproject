#salt tolerance index for morphological traits

x <- data.frame(dplyr::filter(table, table$Treat_Contr=="Treatment"))
x <- x[4:8]
y <- data.frame(dplyr::filter(table, table$Treat_Contr=="Control"))
y <- y[4:8]

STI_Shoot_Length <- x[1]/y[1]
STI_Root_Length <- x[2]/y[2]
STI_Plant_Height <- x[3]/y[3]
STI_Number_Leaves <- x[4]/y[4]
STI_Leaf_Area <- x[5]/y[5]
STI <- data.frame(STI_Shoot_Length,STI_Root_Length, STI_Plant_Height, STI_Number_Leaves, STI_Leaf_Area)

#trying to make a for loop
#STI1 <- data.frame()
#vecteur <- c()
#for (i in names(x)){
#  for (j in 1:4){
#    name <- paste("STI", i ,sep = "_" )
#    name <- x[j]/x[j]
#    vecteur <- c(vecteur, name)}}



#Membership function value for morphological trait 

SL <- numeric()
for (i in 1:25){
  a <- (STI_Shoot_Length[i,1] - min(STI_Shoot_Length))/(max(STI_Shoot_Length)- min(STI_Shoot_Length))
  SL <- c(SL, a)
}

RL <- numeric()
for (i in 1:25){
  a <- (STI_Root_Length[i,1] - min(STI_Root_Length))/(max(STI_Root_Length)- min(STI_Root_Length))
  RL <- c(RL, a)
}

PH <- numeric()
for (i in 1:25){
  a <- (STI_Plant_Height[i,1] - min(STI_Plant_Height))/(max(STI_Plant_Height)- min(STI_Plant_Height))
  PH <- c(PH, a)
}

NL <- numeric()
for (i in 1:25){
  a <- (STI_Number_Leaves[i,1] - min(STI_Number_Leaves))/(max(STI_Number_Leaves)- min(STI_Number_Leaves))
  NL <- c(NL, a)
}

LA <- numeric()
for (i in 1:25){
  a <- (STI_Leaf_Area[i,1] - min(STI_Leaf_Area))/(max(STI_Leaf_Area)- min(STI_Leaf_Area))
  LA <- c(LA, a)
}

MFV <- data.frame(SL, RL, PH, NL, LA)






